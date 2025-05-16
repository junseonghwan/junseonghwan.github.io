import numpy as np
import scipy
from scipy.stats import multivariate_normal
from scipy.special import multigammaln
from numpy.linalg import slogdet

import scipy.stats

from cluster import Cluster

def sample_normal_gamma_prior(dim, shape, scale):
    params = {"mu": np.random.normal(size=dim), 
              "gamma": np.random.gamma(shape, scale, size=dim)}
    return params

# Compute the log likelihood of current configuration.
def log_likelihood(data, params):
    mu = params["mu"]
    gamma = params["gamma"] # this is precision, variance = 1/gamma.
    return(multivariate_normal.logpdf(data, mu, np.diag(1./gamma)).sum())

def propose_new_params(curr_params, gamma_shape=1.0, gamma_scale = 0.5):
    curr_mu = curr_params["mu"]
    curr_gamma = curr_params["gamma"]
    # Local proposal for mu; prior proposal for gamma.
    new_mu = multivariate_normal.rvs(curr_mu, cov=1, size=1)
    new_gamma = np.random.gamma(gamma_shape, gamma_scale, size=len(curr_gamma))
    params = {"mu": new_mu, "gamma": new_gamma}
    return params

def process_samples(datum_to_cluster, parameters):
    cluster_idx = 0
    cluster_to_idx = dict()
    z = np.zeros(len(datum_to_cluster))
    params = []
    for i in range(len(datum_to_cluster)):
        cl = datum_to_cluster[i]
        if cl not in cluster_to_idx:
            cluster_to_idx[cl] = cluster_idx
            cluster_idx += 1
            params.append(parameters[cl])
        z[i] = cluster_to_idx[cl]
    return z, params

def algorithm2(Y, dp_concentration, alpha, theta, gibbs_iter, mh_iter):
    # Initialization.
    N = Y.shape[0]
    dim = Y.shape[1]

    clusters = set() # set of clusters
    datum_to_cluster = [] # index to cluster
    parameters = {} # dictionary: cluster to dict

    # Initialize assignment of datum to cluster.
    cluster = Cluster()
    for n in range(N):
        cluster.add_datum(n)
        datum_to_cluster.append(cluster)
    clusters.add(cluster)
    # Initialize parameters.
    parameters[cluster] = sample_normal_gamma_prior(dim, alpha, theta)

    states = []

    for jj in range(gibbs_iter):
        for datum_idx in range(N):
            curr_cl = datum_to_cluster[datum_idx]
            datum_to_cluster[datum_idx] = None # set it to None to indicate that it is not assigned
            curr_cl.remove_datum(datum_idx) # remove from current cluster
            if curr_cl.size == 0:   # remove curr_cl from clusters.
                if curr_cl in clusters:
                    clusters.remove(curr_cl)
                if curr_cl in parameters:
                    del parameters[curr_cl]
            
            # Compute the probability of assigning datum to each of the clusters.
            temp_clusters = list(clusters)
            K = len(temp_clusters)
            log_probs = np.zeros(K + 1)
            for k, cl in enumerate(temp_clusters):
                log_probs[k] =  np.log(cl.size) -  np.log(N-1+dp_concentration) + log_likelihood(Y[datum_idx,:], parameters[cl])
            # Consider the probability of creating a new cluster.
            # Sample a new parameter value:
            param_star = sample_normal_gamma_prior(dim, alpha, theta)
            log_probs[K] = np.log(dp_concentration) - np.log(N - 1 + dp_concentration) + log_likelihood(Y[datum_idx,:], param_star)
            log_norm = scipy.special.logsumexp(log_probs)
            norm_probs = np.exp(log_probs - log_norm)
            cl_idx = np.random.choice(K+1, p = norm_probs)
            if cl_idx == K:
                #print(f"Assign datum: {datum_idx} to new cluster")
                # we selected a new cluster.
                cluster = Cluster()
                clusters.add(cluster)
                parameters[cluster] = param_star
            else:
                cluster = temp_clusters[cl_idx]
            
            # add datum to cluster and update datum_to_cluster map.
            cluster.add_datum(datum_idx)
            datum_to_cluster[datum_idx] = cluster

        print(f"Number of cluster: {len(clusters)}")
        sum_data_points = 0
        for k, cl in enumerate(clusters):
            sum_data_points += cl.size
            #print(f"Cluster {k} contains {cl.size} data points.")
        print(f"Total data: {sum_data_points}")
        
        # Compute the overall log likelihood.
        log_lik = 0.0
        for cl in clusters:
            log_lik += log_likelihood(Y[cl.data_idxs,:], parameters[cl])
        print(f"Current log likelihood: {log_lik}")
        
        # Update parameters for each cluster using MH.
        log_lik = 0.0
        for cl in clusters:
            curr_params = parameters[cl] 
            for ii in range(mh_iter):
                # propose a new value of the parameters.
                new_params = propose_new_params(curr_params)
                # accept-reject.
                log_lik_star = log_likelihood(Y[cl.data_idxs,:], new_params)
                log_lik_curr = log_likelihood(Y[cl.data_idxs,:], curr_params)
                log_mh_ratio = log_lik_star - log_lik_curr
                log_u = np.log(np.random.uniform(0, 1))
                if log_u < log_mh_ratio: # accepted.
                    curr_params = new_params

            parameters[cl] = curr_params
            log_lik += log_likelihood(Y[cl.data_idxs,:], parameters[cl])
        print(f"Log likelihood after parameter update: {log_lik}")

        # Save the clustering and parameters.
        z, params = process_samples(datum_to_cluster, parameters)
        states.append((z, params))

    return states

def log_predictive(y, prior_params, n, sufficient_stats):
    # For Normal-Wishart, the predictive distribution is given by Multivariate t-distribution.
    sum_y, sum_y_sq = sufficient_stats
    y_bar = sum_y / n
    dim = len(sum_y)

    mu0 = prior_params["mu0"]
    k0 = prior_params["kappa0"]
    nu0 = prior_params["nu0"]
    V0 = prior_params["V0"]

    k_n = k0 + n
    nu_n = nu0 + n
    mu_n = (k0 * mu0 + sum_y) / k_n
    scatter_term = sum_y_sq - n * np.outer(y_bar, y_bar)
    mean_term = ((k0 * n)/(k0 + n)) * np.outer(y_bar - mu0, y_bar - mu0)
    V_n = (V0 + scatter_term + mean_term)
    df = nu_n - dim + 1
    scale = (k_n + 1) / (k_n * df) * V_n
    return scipy.stats.multivariate_t.logpdf(y, df=nu_n - dim + 1, loc=mu_n, shape=scale)

def log_marginal(Y, sum_y, sum_y_sq, prior_params):
    if Y.ndim == 1:
        Y = Y[None, :]  # shape (1, d)
    n, d = Y.shape
    y_bar = sum_y / n

    mu0 = prior_params["mu0"]
    k0 = prior_params["kappa0"]
    nu0 = prior_params["nu0"]
    V0 = prior_params["V0"]

    k_n = k0 + n
    nu_n = nu0 + n
    mu_n = (k0 * mu0 + sum_y) / k_n

    scatter_term = sum_y_sq - n * np.outer(y_bar, y_bar)
    mean_term = (k0 * n) / (k0 + n) * np.outer(y_bar - mu0, y_bar - mu0)
    V_n = V0 + scatter_term + mean_term

    # Log determinants
    sign0, logdetV0 = slogdet(V0)
    signn, logdetVn = slogdet(V_n)
    assert sign0 > 0 and signn > 0  # ensure V is pos-def

    log_Z0 = (
        nu0 / 2 * logdetV0
        + multigammaln(nu0 / 2, d)
    )
    log_Zn = (
        nu_n / 2 * logdetVn
        + multigammaln(nu_n / 2, d)
    )

    log_const = (
        -n * d / 2 * np.log(np.pi)
        + d / 2 * (np.log(k0) - np.log(k_n))
    )

    return log_const + log_Z0 - log_Zn

def process_samples(datum_to_cluster):
    cluster_idx = 0
    cluster_to_idx = dict()
    z = np.zeros(len(datum_to_cluster))
    for i in range(len(datum_to_cluster)):
        cl = datum_to_cluster[i]
        if cl not in cluster_to_idx:
            cluster_to_idx[cl] = cluster_idx
            cluster_idx += 1
        z[i] = cluster_to_idx[cl]
    return z

def algorithm3(Y, dp_concentration, prior_params, gibbs_iter):
    # Initialization.
    N = Y.shape[0]
    
    clusters = set() # set of clusters
    datum_to_cluster = [] # index to cluster
    sufficient_stats = {} # dictionary: cluster to tuple of sum and sum of squares

    # Initialize assignment of datum to cluster.
    cluster = Cluster()
    sufficient_stats[cluster] = (Y.sum(axis=0), Y.T @ Y)
    for n in range(N):
        cluster.add_datum(n)
        datum_to_cluster.append(cluster)
    clusters.add(cluster)

    states = []
    for jj in range(gibbs_iter):
        for datum_idx in range(N):
            sum_y = Y[datum_idx,:]
            sum_y_sq = np.outer(Y[datum_idx,:], Y[datum_idx,:])

            curr_cl = datum_to_cluster[datum_idx]
            datum_to_cluster[datum_idx] = None # set it to None to indicate that it is not assigned
            curr_cl.remove_datum(datum_idx) # remove from current cluster
            # update sufficient stats
            sufficient_stats[curr_cl] = (sufficient_stats[curr_cl][0] - sum_y, sufficient_stats[curr_cl][1] - sum_y_sq)
            if curr_cl.size == 0:   # remove curr_cl from clusters.
                if curr_cl in clusters:
                    clusters.remove(curr_cl)
                if curr_cl in sufficient_stats:
                    del sufficient_stats[curr_cl]

            # Compute the probability of assigning datum to each of the clusters.
            temp_clusters = list(clusters)
            K = len(temp_clusters)
            log_probs = np.zeros(K + 1)
            for k, cl in enumerate(temp_clusters):
                log_probs[k] =  np.log(cl.size) -  np.log(N-1+dp_concentration) + log_predictive(Y[datum_idx,:], prior_params, cl.size, sufficient_stats[cl])
            # Consider the probability of creating a new cluster.
            # Sample a new parameter value:
            log_probs[K] = np.log(dp_concentration) - np.log(N - 1 + dp_concentration) + log_marginal(Y[datum_idx,:], sum_y, sum_y_sq, prior_params)
            log_norm = scipy.special.logsumexp(log_probs)
            norm_probs = np.exp(log_probs - log_norm)
            cl_idx = np.random.choice(K+1, p = norm_probs)
            if cl_idx == K:
                print(f"Assign datum: {datum_idx} to new cluster")
                # we selected a new cluster.
                cluster = Cluster()
                clusters.add(cluster)
                sufficient_stats[cluster] = (0., 0.) # initialize sufficient stats to 0.
            else:
                cluster = temp_clusters[cl_idx]
            
            sufficient_stats[cluster] = (sufficient_stats[cluster][0] + sum_y, 
                                         sufficient_stats[cluster][1] + sum_y_sq)

            # add datum to cluster and update datum_to_cluster map.
            cluster.add_datum(datum_idx)
            datum_to_cluster[datum_idx] = cluster

        print(f"Number of cluster: {len(clusters)}")
        sum_data_points = 0
        for k, cl in enumerate(clusters):
            sum_data_points += cl.size
            sum_y, sum_y_sq = sufficient_stats[cluster]
            #print(f"Cluster {k} suff stats:  {sum_y}, {sum_y_sq}")
            #print(f"Cluster {k} contains {cl.size} data points.")
        print(f"Total data: {sum_data_points}")
        
        # Compute the overall log likelihood.
        log_lik = 0.0
        for cl in clusters:
            sum_y = sufficient_stats[cl][0]
            sum_y_sq = sufficient_stats[cl][1]
            log_lik += log_marginal(Y[cl.data_idxs,:], sum_y, sum_y_sq, prior_params).sum()
        print(f"Current log likelihood: {log_lik}")

        # Save the clustering and parameters.
        z = process_samples(datum_to_cluster)
        states.append(z)

    return states