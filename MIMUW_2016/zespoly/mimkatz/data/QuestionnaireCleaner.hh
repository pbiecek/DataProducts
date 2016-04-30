#ifndef JNPCLEANER_QUESTIONNAIRECLEANER_HH
#define JNPCLEANER_QUESTIONNAIRECLEANER_HH

#include "JnpCleaner.hh"

namespace questionnairecleaner {
    class QuestionnaireCleaner: public jnpcleaner::JnpCleaner {
    public:
        QuestionnaireCleaner() = delete;
        QuestionnaireCleaner(std::string inDir, std::string outDir): jnpcleaner::JnpCleaner::JnpCleaner(inDir, outDir) { };
        QuestionnaireCleaner(const QuestionnaireCleaner& questionnaireCleaner) = delete;
        QuestionnaireCleaner(const QuestionnaireCleaner&& questionnaireCleaner) = delete;
        QuestionnaireCleaner& operator=(const QuestionnaireCleaner&) = delete;
        QuestionnaireCleaner& operator=(const QuestionnaireCleaner&&) = delete;
        ~QuestionnaireCleaner() { };
        bool cleanData();
    };
}

#endif //JNPCLEANER_QUESTIONNAIRECLEANER_HH
