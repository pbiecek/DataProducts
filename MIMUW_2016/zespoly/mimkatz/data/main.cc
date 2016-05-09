#include "GradeCleaner.hh"
#include "QuestionnaireCleaner.hh"

int main(int argc, char** argv) {
    std::string inDirOceny("/home/marcin/jnp2/proj/data/oceny_raw.txt");
    std::string outDirOceny("/home/marcin/jnp2/proj/data/oceny_clean.txt");
    gradecleaner::GradeCleaner gradeCleaner(inDirOceny, outDirOceny);
    if(!gradeCleaner.cleanData())
        return 42;
    std::string inDirAnkiety("/home/marcin/jnp2/proj/data/ankiety_raw.txt");
    std::string outDirAnkiety("/home/marcin/jnp2/proj/data/ankiety_clean.txt");
    questionnairecleaner::QuestionnaireCleaner questionnaireCleaner(inDirAnkiety, outDirAnkiety);
    if(!questionnaireCleaner.cleanData())
        return 43;
    return 0;
}
