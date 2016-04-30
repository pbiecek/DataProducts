#include "QuestionnaireCleaner.hh"

namespace questionnairecleaner {
    bool QuestionnaireCleaner::cleanData() {
        setFiles();
        std::string line;
        int i = 1;
        while(std::getline(inFile_, line, '\n')) {
            std::cout << i << std::endl;
            if(!line.empty() && line[0] != '-' && line[0] != 'K' && line[0] != 'S' && line[line.length() - 1] != '.') {
                std::cout << i << std::endl;
                std::istringstream iss(line);
                std::string code, subjectName, name, surname, type, dydacticCycle, question, answerValue, answerNos, tmp;
                iss >> code;
                std::size_t found = std::string::npos;
                while(found == std::string::npos) {
                    if(iss >> tmp) {
                        found = tmp.find("WYK");
                        if(found == std::string::npos) {
                            found = tmp.find("CW");
                            if(found == std::string::npos) {
                                found = tmp.find("LAB");
                                if(found == std::string::npos)
                                    if(question.empty())
                                        subjectName = tmp;
                                    else
                                        subjectName = subjectName + " " + tmp;
                                else
                                    type = tmp;
                            } else
                                type = tmp;
                        } else
                            type = tmp;
                    } else {
                        std::cerr << line << std::endl;
                        return false;
                    }
                }
                iss >> name >> surname >> answerValue >> answerNos;
                found = std::string::npos;
                while(found == std::string::npos) {
                    if(iss >> tmp) {
                        found = tmp.find("20");
                        if(found == std::string::npos) {
                            found = tmp.find("19");
                            if(found == std::string::npos)
                                if(question.empty())
                                    question = tmp;
                                else
                                    question = question + " " + tmp;
                        } else
                            dydacticCycle = tmp;
                    } else {
                        std::cerr << line << std::endl;
                        return false;
                    }
                }
                std::string formattedLine = code + ";" + subjectName +  ";" + name + " " + surname + ";" + type + ";" + dydacticCycle + ";" + question + ";" + answerValue + ";" + answerNos + "\n";
                setOutputLine(formattedLine);
            }
            i++;
        }
        closeFiles();
        return true;
    }
}