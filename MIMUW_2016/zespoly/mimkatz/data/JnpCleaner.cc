#include "JnpCleaner.hh"

namespace jnpcleaner {
    JnpCleaner::~JnpCleaner() {
        if(inFile_.is_open())
            inFile_.close();
        if(outFile_.is_open())
            outFile_.close();
    }
    void JnpCleaner::setFiles() {
        outFile_.open(outDir_, std::ios::out | std::ios::app);
        inFile_.open(inDir_, std::ios::in);
    }
    void JnpCleaner::closeFiles() {
        if(inFile_.is_open())
            inFile_.close();
        if(outFile_.is_open())
            outFile_.close();
    }
    void JnpCleaner::setOutputLine(std::string line) {
        outFile_ << line;
    }
}
