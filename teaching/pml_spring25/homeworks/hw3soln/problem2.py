import numpy as np
import scipy
import scipy.stats as ss
import pandas as pd
import pdb

def find_root2(sorted_norm_weights, N, verbose=False):
    for j in range(N):
        w_j = sorted_norm_weights[j]
        ss = np.sum(np.minimum(sorted_norm_weights/w_j, 1))
        if ss > N:
            break

    j_star = j
    A_k = j_star
    B_k = np.sum(sorted_norm_weights[j_star:])
    cc = (N - A_k) / B_k

    x = np.minimum(sorted_norm_weights * cc, 1)
    if verbose:
        print(f"A_k: {A_k}, B_k: {B_k}, c: {cc}")
        print(f"sum q_j*c: {np.sum(x)}")
    return cc

def stratified_resample_carpenter(rng, weights, N, L):
    i = 0
    K = np.sum(weights) / (N - L)
    u = rng.uniform(0, K)
    print(f"# of particles to sample:{N-L}, stratum length:{K}, uniform:{u}")
    indices = []
    while i < len(weights):
        u -= weights[i]
        if u < 0:
            # i is selected
            indices.append(i)
            u = u + K
        i += 1
    assert(len(indices) == (N-L))
    return indices

def optimal_resampling(rng, log_weights, N):
    M = len(log_weights)
    log_norm = scipy.special.logsumexp(log_weights)
    normalized_weights = np.exp(log_weights - log_norm)
    if M <= N:
        #return np.array(range(M)), np.repeat(1./M, M) # all indices survive.
        return np.array((range(M))), normalized_weights
    
    # Perform resampling proposed in Fearnhead and Clifford (2003).
    original_indices = np.argsort(-normalized_weights)
    sorted_weights = normalized_weights[original_indices]

    # Work with sorted weights/indices.
    c = find_root2(sorted_weights, N)
    cw = c * sorted_weights
    
    # Any particle where cw >= 1 are automatically selected.
    selected_particles_idxs = cw >= 1
    A_idx = original_indices[selected_particles_idxs]
    B_idx = None

    # Determine if we need to do additional sampling.
    remaining_idx = ~selected_particles_idxs
    # Perform stratified sampling on the remaining elements.
    L = np.sum(selected_particles_idxs == True)
    if (N-L) > 0:
        remaining_original = original_indices[remaining_idx]
        stratified_sampled_idxs = stratified_resample_carpenter(rng, sorted_weights[remaining_idx], N, L)
        # Convert back to original indices.
        B_idx = remaining_original[stratified_sampled_idxs]
        #B_idx = original_indices[remaining_idx][stratified_sampled_idxs]
        combined_idxs = np.concatenate((A_idx, B_idx))
        weights = np.concatenate((normalized_weights[A_idx], np.repeat(1/c, N-L))) if L > 0 else np.repeat(1/c, N)
    else:
        combined_idxs = A_idx
        weights = normalized_weights
    assert np.isclose(np.sum(weights), 1.)
    return combined_idxs, weights

class WellLogState():
    def __init__(self, suff, n, s, o, tau, parent):
        self._suff = suff
        self._n = n
        self._s = s
        self._o = o
        self._tau = tau
        self._parent = parent

    @property
    def parent(self):
        return self._parent
    
    @property
    def suff(self):
        return self._suff

    @property
    def o(self):
        return self._o

    @property
    def s(self):
        return self._s

    @property
    def n(self):
        return self._n

    @property
    def tau(self):
        return self._tau

    def mean_sd(self, mu, sigma, tau_1):
        var = 1./(1./sigma**2 + self._n/tau_1**2)
        mean = var * (mu / sigma**2 + self._suff/tau_1**2)
        return mean, np.sqrt(var)

class ParticlePopulation:
    def __init__(self, particles, weights):
        self._particles = particles
        self._weights = weights

    def at(self, idx):
        return self._particles[idx], self._weights[idx]

    @property
    def particles(self):
        return self._particles

    @property
    def weights(self):
        return self._weights

    def size(self):
        return len(self._particles)
    
def run_pf(Y, max_particles=100, mu=115000, sigma=10000, nu=85000, tau_1=2500, tau_2=12500, cp_prob = 0.00001):
    # Well log model keeps track of mean, variance of X_t, S_t, O_t, and \tau.
    # P(S_t = 2) = cp_prob.
    transition1 = np.array([[1 - cp_prob, cp_prob], [1-cp_prob, cp_prob]])

    # Transition from O_{t-1} to O_t
    transition2 = np.array([[0.996, 0.04], [0.25, 0.75]])

    chain_length = len(Y)
    states = [(1, 1), (1, 2), (2, 1), (2, 2)]

    rng = np.random.default_rng(441)
    null_state = WellLogState(0, 0, 1, 1, 0, None)
    initial_pop = ParticlePopulation([null_state], [1])
    pops = []
    #print(initial_pop.size())

    for t in range(chain_length):
        print(f"Iteration {t}.")
        y = Y[t]
        particle_pop = initial_pop if t == 0 else pops[t-1]
        particles = []
        log_weights = []
        num_particles = np.min([max_particles, particle_pop.size()])
        #pdb.set_trace()
        for particle_idx in range(num_particles):
            old_state, old_weight = particle_pop.at(particle_idx)
            # Enumerate all four states.
            for s, o in states:
                if s == 1:
                    tau = old_state.tau
                    suff = old_state.suff
                    segment_length = old_state.n
                    if o == 1:
                        suff += y
                        segment_length += 1
                else:
                    tau = t
                    if o == 1:
                        suff = y
                        segment_length = 1
                    else:
                        suff = 0
                        segment_length = 0
                
                new_state = WellLogState(suff, segment_length, s, o, tau, old_state)
                if s == 1 and o == 1:
                    mean, sd = old_state.mean_sd(mu, sigma, tau_1)
                    sd = np.sqrt(sd ** 2 + tau_1 **2)
                elif s == 2 and o == 1:
                    mean = mu
                    sd = np.sqrt(sigma ** 2 + tau_1 ** 2)
                else: # o == 2
                    mean, sd = nu, tau_2
                new_log_weight = np.log(old_weight)
                new_log_weight += np.log(transition1[old_state.s-1, s-1])
                new_log_weight += np.log(transition2[old_state.o-1,o-1]) 
                new_log_weight += ss.norm.logpdf(y, mean, sd)
                particles.append(new_state)
                log_weights.append(new_log_weight)

        # Perform resampling.
        #pdb.set_trace()
        idxs, weights = optimal_resampling(rng, log_weights, max_particles)
        new_particles = [particles[idx] for idx in idxs]
        new_pop = ParticlePopulation(new_particles, weights)
        pops.append(new_pop)
    return pops

def main():
    max_particles = 10
    mu = 115000
    sigma = 10000
    nu = 85000
    tau_1 = 2500
    tau_2 = 12500
    cp_prob = 1./250

    well_data = pd.read_csv("welldata.csv", header=None)
    Y = well_data.to_numpy()[:,0]

    pops = run_pf(Y, max_particles, mu, sigma, nu, tau_1, tau_2, cp_prob)
    # Process pops.

if __name__ == "__main__":
    main()
