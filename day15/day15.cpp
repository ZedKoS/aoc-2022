
#include <algorithm>
#include <array>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_set>
#include <vector>

using namespace std;

// we do a little trolling :)
#define int int64_t

struct Point { int x, y; };
struct Line { Point start, end; };
struct Range { int start, end; };
struct Sensor { Point sensor, beacon; };


size_t size(Range r) {
    return abs(r.end - r.start);
}

struct PointHash {
    size_t operator()(const Point& a) const {
        constexpr int idk = 429131;
        return abs(a.x) * idk + abs(a.y);
    }
};

inline bool operator==(Point const& a, Point const& b) {
    return (a.x == b.x) && (a.y == b.y);
}
inline bool operator!=(Point const& a, Point const& b) {
    return !operator==(a, b);
}


vector<Sensor> read_input(string_view filename) {
    ifstream input(filename.data());

    constexpr char format[] = "Sensor at x=%lld, y=%lld: closest beacon is at x=%lld, y=%lld";
    vector<Sensor> out;

    string line;
    while (!input.eof()) {
        Sensor s;
        getline(input, line);

        sscanf(line.data(), format, &s.sensor.x, &s.sensor.y, &s.beacon.x, &s.beacon.y);
        out.push_back(s);
        line.clear();
    }
    
    return out;
}


int64_t solve1(vector<Sensor> const& in, int target_row = 2'000'000);
int64_t solve2(vector<Sensor> const& in);

inline int manhattan(Point a, Point b) {
    return abs(a.x - b.x) + abs(a.y - b.y);
}


int32_t main() {
    vector<Sensor> in = read_input("input.txt");

    auto sol1 = solve1(in);
    cout << "Part 1: " << sol1 << endl;

    auto sol2 = solve2(in);
    cout << "Part 2: " << sol2 << endl;
}


void row_ranges(vector<Sensor> const& sensors, vector<Range>& ranges, int target_row) {
    // construct ranges
    for (auto s : sensors) {
        auto dy = abs(s.sensor.y - target_row);
        auto radius = manhattan(s.sensor, s.beacon);

        auto row_size = 2 * (radius - dy) + 1;

        if (row_size <= 0)
            continue;

        ranges.push_back({ s.sensor.x - row_size / 2, s.sensor.x + row_size / 2 + 1 });
    }

    sort(ranges.begin(), ranges.end(), [] (Range a, Range b) {
        return a.end < b.end;
    });


    // merge ranges
    for (auto it = ranges.rbegin(); it < ranges.rend() - 1; it++) {
        auto& fst = *(it + 1);
        auto& snd = *it;

        if (snd.start <= fst.end) {
            auto start = min(fst.start, snd.start);
            auto end = max(fst.end, snd.end);
            fst = { start, end };
            ranges.erase(--it.base());
        }
    }
}

int64_t solve1(vector<Sensor> const& sensors, int target_row) {
    vector<Range> ranges(8);
    row_ranges(sensors, ranges, target_row);

    int excluded_len = 0;
    for (Range r : ranges) {
        excluded_len += size(r);
    }

    unordered_set<Point, PointHash> beacons;
    for (Sensor s : sensors) {
        if (s.beacon.y == target_row)
            beacons.insert(s.beacon);
    }

    return excluded_len - beacons.size();
}


void scan_rows(vector<Sensor> const& in, int from, int to, Point& result) {
    vector<Range> ranges(16);

    for (int row = from; row < to; row++) {
        row_ranges(in, ranges, row);

        for (Range r : ranges) {
            if (0 <= r.start - 1 && r.start - 1 <= 4'000'000) {
                auto x = r.start - 1;
                auto y = row;

                result = { x, y };
                return;
            }
        }

        ranges.clear();
    }

    result = { -1, -1 };
}

int solve2(vector<Sensor> const& in) {
    constexpr size_t ROWS = 4'000'000;
    constexpr size_t JOBS = 8;

    static_assert(ROWS % JOBS == 0);

    thread jobs[JOBS];
    Point points[JOBS];

    for (size_t i = 0; i < JOBS; i++) {
        auto start = i * (ROWS / JOBS);
        auto end = start + ROWS / JOBS;
        jobs[i] = thread(scan_rows, in, start, end, ref(points[i]));
    }
    
    Point p = { -1, -1 };
    for (size_t i = 0; i < JOBS; i++) {
        jobs[i].join();
        if (!(points[i] == Point{-1,-1})) {
            p = points[i];
        }
    }

    return p.x * 4'000'000 + p.y;
}