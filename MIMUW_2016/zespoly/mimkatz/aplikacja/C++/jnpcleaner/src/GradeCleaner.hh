#ifndef JNPCLEANER_GRADECLEANER_HH
#define JNPCLEANER_GRADECLEANER_HH

#include "JnpCleaner.hh"

//Specific implementation of the jnp cleaner for cleaning grade related data
namespace gradecleaner {
    class GradeCleaner: public jnpcleaner::JnpCleaner {
    public:
        GradeCleaner() = delete;
        GradeCleaner(std::string inDir, std::string outDir): jnpcleaner::JnpCleaner::JnpCleaner(inDir, outDir) { };
        GradeCleaner(const GradeCleaner& gradeCleaner) = delete;
        GradeCleaner(const GradeCleaner&& gradeCleaner) = delete;
        GradeCleaner& operator=(const GradeCleaner& gradeCleaner) = delete;
        GradeCleaner& operator=(const GradeCleaner&& gradeCleaner) = delete;
        ~GradeCleaner() { };
        bool cleanData();
    };
}

#endif //JNPCLEANER_GRADECLEANER_HH
