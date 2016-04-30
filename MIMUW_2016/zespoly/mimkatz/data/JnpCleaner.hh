#ifndef JNPCLEANER_JNPCLEANER_HH
#define JNPCLEANER_JNPCLEANER_HH

#include <iostream>
#include <fstream>
#include <cstring>
#include <sstream>

namespace jnpcleaner {
    class JnpCleaner {
        std::string inDir_;
        std::string outDir_;
    protected:
        std::ifstream inFile_;
        std::ofstream outFile_;
    public:
        JnpCleaner() = delete;
        JnpCleaner(std::string inDir, std::string outDir): inDir_(inDir), outDir_(outDir) { };
        JnpCleaner(const JnpCleaner&) = delete;
        JnpCleaner(const JnpCleaner&&) = delete;
        JnpCleaner& operator=(const JnpCleaner&) = delete;
        JnpCleaner& operator=(const JnpCleaner&&) = delete;
        virtual ~JnpCleaner();
        void setFiles();
        void closeFiles();
        void setOutputLine(std::string);
        virtual bool cleanData() = 0;
    };
}

#endif //JNPCLEANER_JNPCLEANER_HH
