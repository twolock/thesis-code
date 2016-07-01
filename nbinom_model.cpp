#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_MATRIX(X);
  DATA_VECTOR(Y);
  DATA_VECTOR(offset);
  DATA_IVECTOR(s_i);
  DATA_IVECTOR(c_i);
  DATA_IVECTOR(s_c_i);
  DATA_SCALAR(theta_u);
  DATA_SCALAR(theta_l)

  PARAMETER(logit_theta);
  PARAMETER(C);
  PARAMETER_VECTOR(B);
  PARAMETER_VECTOR(B_s);
  PARAMETER(log_SD_s);
  PARAMETER_VECTOR(B_c);
  PARAMETER(log_SD_c);

  max_parallel_regions = omp_get_max_threads(); 

  // parallel_accumulator<Type> jnll(this);

  Type theta = (exp(logit_theta)*theta_u + theta_l)/(Type(1)+exp(logit_theta));

  Type jnll = 0.0;

  vector<Type> lin_pred = C + X*B;
  for (int i = 0; i < Y.size(); ++i)
    lin_pred(i) += B_s(s_i(i)) + B_c(c_i(i));

  vector<Type> pred_count = exp(lin_pred) * offset;

  vector<Type> var = pred_count + (pred_count*pred_count)/theta;

  for (int i = 0; i < Y.size(); ++i)
    PARALLEL_REGION jnll -= dnbinom2(Y(i), pred_count(i), var(i), true);
  for (int i = 0; i < B_s.size(); ++i)
    PARALLEL_REGION jnll -= dnorm(B_s(i), Type(0), exp(log_SD_s), true);
  for (int i = 0; i < B_c.size(); ++i)
    PARALLEL_REGION jnll -= dnorm(B_c(i), Type(0), exp(log_SD_c), true);
  
  // jnll -= dgamma(exp(log_theta), Type(1), Type(1), true);

  REPORT(pred_count);

  return jnll;
}
