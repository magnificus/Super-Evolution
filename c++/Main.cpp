#include<iostream>
#include<vector>
#include<map>
#include<functional>
#include<algorithm>
#include "EvolutionTypes.h"

int NumTranslationUnits = 100;
int NumAlternatives = 100;

int main(){

	srand (time(NULL));

	std::map<std::string, double> Values = {std::make_pair("x", 1.0)};

	std::vector<Solution> Solutions;

	Solutions.push_back(Solution{Values, 1.0});
	
	std::vector<std::string> ValueStrings = {"x"};
	std::vector<Alternative> Alternatives;

	for (int i = 0; i < NumAlternatives; i++)
		Alternatives.push_back(Alternative(ValueStrings,  NumTranslationUnits));
	
		///Alternatives.push_back(TranslationUnit::GetRandomTranslationUnit(ValueStrings, NumUnits));

	for (Alternative &A : Alternatives)
		A.Fitness = A.GetFitnessForSolutions(Solutions);

	int count = 0;
	while (true){
		// the main loop

		std::cout << std::endl << std::endl;
		//std::cout << count++;
		for (int i = 0; i < Alternatives.size(); i++){
			std::cout << "Tree: " << Alternatives[i].GetTree() << std::endl;
			std::cout << "Result: " << Alternatives[i].GetValue(Values) << std::endl;
			std::cout << "Fitness: " << Alternatives[i].GetFitnessForSolutions(Solutions) << std::endl;
		}

		std::sort(Alternatives.begin(), Alternatives.end(), [](const Alternative &T1,const Alternative &T2) { return T1.Fitness < T2.Fitness; });
	}


	return 0;


}
