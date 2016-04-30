#include "GradeCleaner.hh"

namespace gradecleaner {
    bool GradeCleaner::cleanData() {
        setFiles();
        std::string line;
        int i = 1;
        while(std::getline(inFile_, line, '\n')) {
            if(!line.empty() && line[0] != '-' && line[0] != 'K' && line[0] != 'S' && line[line.length() - 1] != '.') {
                std::cout << i << std::endl;
                std::istringstream iss(line);
                std::string code, name, dydacticCycle, term, grade, person, tmp;
                iss >> code;
                std::size_t found = std::string::npos;
                while(found == std::string::npos) {
                    if(iss >> tmp) {
                        found = tmp.find("20");
                        if(found == std::string::npos) {
                            found = tmp.find("19");
                            if(found == std::string::npos)
                                if(name.empty())
                                    name = tmp;
                                else
                                    name = name + " " + tmp;
                            else
                                dydacticCycle = tmp;
                        } else
                            dydacticCycle = tmp;
                    } else {
                        std::cerr << line << std::endl;
                        return false;
                    }
                }
                iss >> term;
                iss >> grade >> person;
                std::string formattedLine = code + ";" + name + ";" + dydacticCycle + ";" + term + ";" + grade + ";" + person + "\n";
                setOutputLine(formattedLine);
            }
            i++;
        }
        closeFiles();
        return true;
    }
}