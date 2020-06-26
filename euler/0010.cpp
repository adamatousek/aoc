#include <array>
#include <iostream>

constexpr int primes_below = 2'000'000;
constexpr int bound = primes_below - 2;
using sieve = std::array<bool, bound + 1>;

int main() {
    sieve s = {};
    long sum = 0;
    int n = 2; // i = 0
    while ( true ) {
        while ( s[n-2] )
            ++n;
        if ( n - 2 == bound )
            break;
        sum += n;
        for ( int i = n - 2; i < bound; i += n )
            s[i] = true;
    }
    std::cout << "Sum = " << sum << std::endl;
    return 0;
}
