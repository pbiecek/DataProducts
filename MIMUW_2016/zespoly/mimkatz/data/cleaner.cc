#include <iostream>
#include <fstream>
#include <cstring>
#include <sstream>

int main(int argc, char** argv) {
    std::ifstream inFile;
    std::ofstream outFile;
    std::string inDir("/home/marcin/jnp2/proj/data/oceny.txt");
    std::string outDir("/home/marcin/jnp2/proj/data/ocenyout.txt");
    outFile.open(outDir, std::ios::out | std::ios::app);
    inFile.open(inDir, std::ios::in);
    std::string line;
    int i = 1;
    while(std::getline(inFile, line, '\n')) {
        if(!line.empty() && line[0] != '-' && line[0] != 'K' && line[0] != 'S') {
            //std::cout << i << std::endl;
            std::istringstream iss(line);
            std::string code, name, dyd_cycle, term, grade, person, tmp;
            iss >> code;
            std::size_t found;
            do {
                if(iss >> tmp) {
                    found = tmp.find("20");
                    if(found == std::string::npos) {
                        found = tmp.find("19");
                        if(found == std::string::npos)
                            if(name.empty())
                                name = tmp;
                            else
                                name = name + " " + tmp;
                    } else
                        dyd_cycle = tmp;
                } else
                    return 42;
            } while (found == std::string::npos);
            iss >> term;
            iss >> grade >> person;
            std::string formattedLine = code + ";" + name + ";" + dyd_cycle + ";" + term + ";" + grade + ";" + person + "\n";
            outFile << formattedLine;
        }
        i++;
    }
    inFile.close();
    outFile.close();
    return 0;
}
