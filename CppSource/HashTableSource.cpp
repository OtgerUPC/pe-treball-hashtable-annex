#include "MurmurHash3.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
uint32_t KRHash(String word) {
  const char* letters = word.get_cstring();
  uint32_t hash_value = 0;
  
  while (*letters != 0)
    hash_value = 31 * hash_value + *letters++;
  return hash_value;
}

// [[Rcpp::export]]
uint32_t MurmurHash(String word) {
  const uint32_t seed = 12345678;
  
  const char* letters = word.get_cstring();
  uint32_t hash_value;
  
  MurmurHash3_x86_32(letters, strlen(letters), seed, &hash_value);
  
  return hash_value;
}

void AssertHash(int hash, int expected_hash) {
  if (hash != expected_hash)
    stop("Hash function is not well implemented");
}


