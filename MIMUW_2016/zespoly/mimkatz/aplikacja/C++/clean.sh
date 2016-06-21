#!/bin/bash
gradesInPath=""
gradesOutPath=""
questionsInPath=""
questionsOutPath=""
which="both" #inne możliwe wartości to "grades" lub "questions"

./jnpcleaner $which $gradesInPath $gradesOutPath $questionsInPath $questionsOutPath;
