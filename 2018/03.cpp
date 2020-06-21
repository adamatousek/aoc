#include <iostream>
#include <vector>

#define FSZ 1000
char fabric[ FSZ * FSZ ] = {0};

struct Alloc {
    short x, y, w, h, id;
    Alloc( short x, short y, short w, short h, short id )
        : x( x ), y( y ), w( w ), h( h ), id( id ) {}
};

int main()
{
    int n_overlap = 0;
    std::vector< Alloc > allocs;
    allocs.reserve( 1247 );
    std::cin.get();
    while (! std::cin.eof()) {
        unsigned id, x, y, w, h;
        char sep;
        std::cin >> id >> sep >> x >> sep >> y >> sep >> w >> sep >> h;
        allocs.emplace_back( x, y, w, h, id );
        for ( int iy = y; iy < y + h; ++iy ) {
            for ( int ix = x; ix < x + w; ++ix ) {
                char & sq = fabric[iy * FSZ + ix];
                switch (sq) {
                case 1:
                    ++n_overlap;
                case 0:
                    sq += 1;
                default:
                    break;
                }
            }
        }
        std::cin >> sep;
    }

    std::cout << allocs.size() << " allocs: " << n_overlap << " overlaps" << std::endl;
    std::cout << "Safe allocs:";

    for ( const auto &al : allocs ) {
        bool safe = true;
        for ( int iy = al.y; iy < al.y + al.h; ++iy ) {
            for ( int ix = al.x; ix < al.x + al.w; ++ix ) {
                char & sq = fabric[iy * FSZ + ix];
                safe = safe && ( sq == 1 );
            }
        }
        if (safe)
            std::cout << ' ' << al.id;
    }
    std::cout << std::endl;
}
