#ifndef JNPCLEANER_JNPCLEANER_HH
#define JNPCLEANER_JNPCLEANER_HH

#include <iostream>
#include <fstream>
#include <cstring>
#include <sstream>

//A general ABC class used for cleaning jnp2 USOS sql data.
//Packed into a namespace.
namespace jnpcleaner {
    class JnpCleaner {
        std::string inDir_;
        std::string outDir_;
    protected:
        std::ifstream inFile_;
        std::ofstream outFile_;
    public:
        //We delete most constructors, copy constructors, move constructors and related assignment operators
        JnpCleaner() = delete;
        //The only used constructor - takes input and output directory
        JnpCleaner(std::string inDir, std::string outDir): inDir_(inDir), outDir_(outDir) { };
        JnpCleaner(const JnpCleaner&) = delete;
        JnpCleaner(const JnpCleaner&&) = delete;
        JnpCleaner& operator=(const JnpCleaner&) = delete;
        JnpCleaner& operator=(const JnpCleaner&&) = delete;
        //Virtual destructor
        virtual ~JnpCleaner();
        //File setter, no need for a getter though
        void setFiles();
        //Closes files
        void closeFiles();
        //Prints an output line to the output file
        void setOutputLine(std::string);
        //Virtual function that cleans the data
        virtual bool cleanData() = 0;
    };
}

#endif //JNPCLEANER_JNPCLEANER_HH
