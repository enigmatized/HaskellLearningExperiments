#include <vector>

class CorrelationMatrix {
private:
    std::vector<double> data;  // Packed triangular matrix data
    size_t size;               // Number of variables

    // Convert matrix indices to packed array index
    size_t getIndex(size_t i, size_t j) const {
        // Assuming i <= j, convert (i, j) to linear index
        return (j * (j - 1) / 2) + i;
    }

public:
    // Constructor to initialize the matrix
    explicit CorrelationMatrix(size_t size) : size(size) {
        // Calculate the size of packed array based on number of variables
        data.resize((size * (size - 1)) / 2);
    }

    // Set correlation value for given indices (i, j)
    void setCorrelation(size_t i, size_t j, double value) {
        if (i == j) {
            // Diagonal elements are not stored in the packed triangular matrix
            return;
        }

        if (i > j) {
            // Swap indices to ensure i <= j
            std::swap(i, j);
        }

        // Get the index in the packed array
        size_t index = getIndex(i, j);

        // Set the correlation value
        data[index] = value;
    }

    // Get correlation value for given indices (i, j)
    double getCorrelation(size_t i, size_t j) const {
        if (i == j) {
            // Diagonal elements are assumed to be 1.0
            return 1.0;
        }

        if (i > j) {
            // Swap indices to ensure i <= j
            std::swap(i, j);
        }

        // Get the index in the packed array
        size_t index = getIndex(i, j);

        // Get the correlation value
        return data[index];
    }

    // Get the packed triangular matrix data
    const std::vector<double>& getCorrelationData() const {
        return data;
    }
};

// Extern "C" wrapper for creating the CorrelationMatrix object
extern "C" CorrelationMatrix* createCorrelationMatrix(size_t size) {
    return new CorrelationMatrix(size);
}

// Extern "C" wrapper for setting the correlation value
extern "C" void setCorrelation(CorrelationMatrix* matrix, size_t i, size_t j, double value) {
    matrix->setCorrelation(i, j, value);
}

// Extern "C" wrapper for getting the correlation value
extern "C" double getCorrelation(CorrelationMatrix* matrix, size_t i, size_t j) {
    return matrix->getCorrelation(i, j);
}

// Extern "C" wrapper for getting the packed triangular matrix data
extern "C" const double* getCorrelationData(CorrelationMatrix* matrix) {
    const std::vector<double>& data = matrix->getCorrelationData();
    return data.data();
}

// Extern "C" wrapper for deleting the CorrelationMatrix object
extern "C" void deleteCorrelationMatrix(CorrelationMatrix* matrix) {
    delete matrix;
}

