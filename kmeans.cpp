#include <iostream>
#include <vector>
#include <cmath>
#include <limits>

using namespace std;

class Point {
public:
    vector<double> coords;

    Point(const vector<double>& coords = {}) : coords(coords) {}
};

class Cluster {
public:
    Point center;
    vector<Point> points;
    int dimensions;

    //constructor
    Cluster(const Point& center) : center(center), dimensions(center.coords.size()) {}

    void addPoint(const Point& p) {
        points.push_back(p);
    }

    void clear() {
        points.clear();
    }

    //updates center of cluster
    void update() {
        vector<double> sum(dimensions, 0);
        for (const auto& p : points) {
            for (int i = 0; i < dimensions; ++i) {
                sum[i] += p.coords[i];
            }
        }
        for (int i = 0; i < dimensions; ++i) {
            center.coords[i] = sum[i] / points.size();
        }
    }

    bool isEmpty() const {
        return points.empty();
    }
};

double distance(const Point& p1, const Point& p2) {
    double sum = 0;
    for (int i = 0; i < p1.coords.size(); ++i) {
        double d = p1.coords[i] - p2.coords[i];
        sum += d * d;
    }
    return sqrt(sum);
}

int findNearestCluster(const vector<Cluster>& clusters, const Point& p) {
    int index = 0;
    double minDistance = numeric_limits<double>::max();
    for (int i = 0; i < clusters.size(); ++i) {
        double d = distance(clusters[i].center, p);
        if (d < minDistance) {
            index = i;
            minDistance = d;
        }
    }
    return index;
}

vector<Cluster> kmeans(const vector<Point>& points, int k, int maxIterations) {
    vector<Cluster> clusters(k, Cluster(Point(vector<double>(points[0].coords.size(), 0))));

    // Initialize clusters with random points
    for (int i = 0; i < k; ++i) {
        int randomIndex = rand() % points.size();
        clusters[i].center = points[randomIndex];
    }

    // Assign points to initial clusters
    for (const auto& p : points) {
        int nearestCluster = findNearestCluster(clusters, p);
        clusters[nearestCluster].addPoint(p);
    }

    // Update clusters until convergence or maxIterations is reached
    int iteration = 0;
    while (iteration < maxIterations) {
        bool converged = true;
        for (auto& c : clusters) {
            if (!c.isEmpty()) {
                Point oldCenter = c.center;
                c.update();
                if (distance(oldCenter, c.center) > 1e-9) {
                    converged = false;
                }
            }
        }
        if (converged) {
            break;
        }
	//clear out clusters and reassin points
        for (auto& c : clusters) {
            c.clear();
        }
        for (const auto& p : points) {
            int nearestCluster = findNearestCluster(clusters, p);
            clusters[nearestCluster].addPoint(p);
        }
        ++iteration;
    }

    return clusters;
}

int main() {
    vector<Point> points = {Point({1, 2, -3}), Point({3, 4, 0}), Point({5, 6, 1}), Point({7, 3, -1})};
    int k = 2;
    int maxIterations = 100;
	
    // Run k-means clustering
	vector<Cluster> clusters = kmeans(points, k, maxIterations);
	
	// Print cluster centers and points
	for (int i = 0; i < clusters.size(); ++i) {
		cout << "Center of cluster " << i << ": (";
		for (int j = 0; j < clusters[i].center.coords.size(); ++j) {
			if (j > 0) {
				cout << ", ";
			}
			cout << clusters[i].center.coords[j];
		}
		cout << ")" << endl;
		cout << "Points in cluster " << i << ":" << endl;
		for (const auto& p : clusters[i].points) {
			cout << "(";
			for (int j = 0; j < p.coords.size(); ++j) {
				if (j > 0) {
					cout << ", ";
				}
				cout << p.coords[j];
			}
			cout << ")" << endl;
		}
	}

	return 0;

}
