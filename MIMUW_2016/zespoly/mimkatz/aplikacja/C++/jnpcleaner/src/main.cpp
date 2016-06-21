#include "GradeCleaner.hh"
#include "QuestionnaireCleaner.hh"

int main(int argc, char** argv) {
    if(argc < 2)
        return 40;
    std::string whatAreWeCleaning = std::string(argv[1]);
    if(whatAreWeCleaning.compare("both") != 0 && whatAreWeCleaning.compare("grades") != 0 && whatAreWeCleaning.compare("questions") != 0)
        return 41;
    if(whatAreWeCleaning.compare("both") == 0) {
        if(argc != 6)
            return 42;
        std::string inDirOceny(argv[2]);
        std::string outDirOceny(argv[3]);
        gradecleaner::GradeCleaner gradeCleaner(inDirOceny, outDirOceny);
        if(!gradeCleaner.cleanData())
            return 45;
        std::string inDirAnkiety(argv[4]);
        std::string outDirAnkiety(argv[5]);
        questionnairecleaner::QuestionnaireCleaner questionnaireCleaner(inDirAnkiety, outDirAnkiety);
        if(!questionnaireCleaner.cleanData())
            return 46;
    } else if(whatAreWeCleaning.compare("grades") == 0) {
        if(argc != 4)
            return 43;
        std::string inDirOceny(argv[2]);
        std::string outDirOceny(argv[3]);
        gradecleaner::GradeCleaner gradeCleaner(inDirOceny, outDirOceny);
        if(!gradeCleaner.cleanData())
            return 45;
    } else {
        if(argc != 4)
            return 44;
        std::string inDirAnkiety(argv[2]);
        std::string outDirAnkiety(argv[3]);
        questionnairecleaner::QuestionnaireCleaner questionnaireCleaner(inDirAnkiety, outDirAnkiety);
        if(!questionnaireCleaner.cleanData())
            return 46;
    }
    return 0;
}