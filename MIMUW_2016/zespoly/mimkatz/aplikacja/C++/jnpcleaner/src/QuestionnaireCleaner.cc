#include "QuestionnaireCleaner.hh"

namespace questionnairecleaner {
    bool QuestionnaireCleaner::cleanData() {
        setFiles();
        std::string line;
        int i = 1;
        setOutputLine("KOD;PRZ_NAZWA;PROWADZACY;KOD_ZAJ;CDYD_KOD;TRESC_PYTANIA;WARTOSC_ODPOWIEDZI;LICZBA_ODPOWIEDZI\n");
        while(std::getline(inFile_, line, '\n')) {
            std::cout << i << std::endl;
            if(!line.empty() && line[0] != '-' && line[0] != 'K' && line[0] != 'S' && line[line.length() - 1] != '.') {
                std::cout << i << std::endl;
                std::istringstream iss(line);
                std::string code, subjectName, name, surname, fullName, type, dydacticCycle, question, answerValue, answerNos, tmp;
                iss >> code;
                std::size_t found = std::string::npos;
                while(found == std::string::npos) {
                    if(iss >> tmp) {
                        found = tmp.find("WYK");
                        if(found == std::string::npos) {
                            found = tmp.find("CW");
                            if(found == std::string::npos) {
                                found = tmp.find("LAB");
                                if(found == std::string::npos) {
                                    found = tmp.find("SEM-MGR");
                                    if (found == std::string::npos) {
                                        found = tmp.find("WYK-MON");
                                        if (found == std::string::npos)
                                            if (subjectName.empty())
                                                subjectName = tmp;
                                            else
                                                subjectName = subjectName + " " + tmp;
                                        else
                                            type = tmp;
                                    } else
                                        type = tmp;
                                } else
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
                found = std::string::npos;
                while(found == std::string::npos) {
                    if(iss >> tmp) {
                        if(isdigit(tmp[0]))
                            found = 1;
                        else
                            found = std::string::npos;
                        if(found == std::string::npos) {
                            if (fullName.empty())
                                fullName = tmp;
                            else
                                fullName = fullName + " " + tmp;
                        } else {
                            answerValue = tmp;
                        }
                    } else {
                        std::cerr << line << std::endl;
                        return false;
                    }
                }
                iss >> answerNos;
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
                std::string formattedLine = code + ";" + subjectName +  ";" + fullName + ";" + type + ";" + dydacticCycle + ";" + question + ";" + answerValue + ";" + answerNos + "\n";
                setOutputLine(formattedLine);
            }
            i++;
        }
        closeFiles();
        return true;
    }
}