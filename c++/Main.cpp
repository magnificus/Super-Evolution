#include<iostream>
#include<vector>
#include<map>
#include "EvolutionTypes.h"

int NumUnits = 1000;

int main(){

	srand (time(NULL));

	std::map<std::string, double> Values = {std::make_pair("x", 1.0)};
	
	std::vector<std::string> ValueStrings = {"x"};
	std::vector<TranslationUnit> TranslationUnits;

	for (int i = 0; i < NumUnits; i++)
		TranslationUnits.push_back(TranslationUnit::GetRandomTranslationUnit(ValueStrings, NumUnits));

	std::cout << "Tree: " << TranslationUnits[0].GetTree(2, TranslationUnits) << std::endl;
	std::cout << "Result: " << TranslationUnits[0].GetValue(2, Values, TranslationUnits) << std::endl;

	return 0;


}
