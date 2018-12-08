#include<iostream>
#include<vector>
#include<map>
#include<functional>
#include<algorithm>
#include "EvolutionTypes.h"

int NumTranslationUnits = 200;
int NumAlternatives = 10000;

float CullRatio = 0.15;

void NextGeneration(std::vector<Alternative> &Alternatives, std::vector<Solution> &Solutions, std::vector<std::string> &ValueStrings){
	for (int i = 0; i < Alternatives.size(); i++){
		bool ShouldCull = CullRatio*(float(i) / Alternatives.size()) > fRand(0.0, 2.0);
		if (ShouldCull){
			int Parent1 = rand() % Alternatives.size();
			int Parent2 = rand() % Alternatives.size();

			Alternatives[i] = Alternatives[Parent1];
			//Alternatives[i] = Alternative::GetChild(Alternatives[Parent1], Alternatives[Parent2]);
			Alternatives[i].Mutate(ValueStrings);
			Alternatives[i].Fitness = Alternatives[i].GetFitnessForSolutions(Solutions);
		}
	}
}

int main(){

	srand (time(NULL));

	std::vector<Solution> Solutions;


	double MinInputValue = 0.0;
	double MaxInputValue = 100.0;

	// only x
	for (double x = MinInputValue; x < MaxInputValue; x++){
		std::map<std::string, double> ValuesMap = {std::make_pair("x", x)};
		Solutions.push_back(Solution{ValuesMap, pow(x,2.1) + 63.9});
	}

	// x and y
//	for (double x = MinInputValue; x < MaxInputValue; x++){
		//for (double y = MinInputValue; y < MaxInputValue; y++){
			//std::map<std::string, double> ValuesMap = {std::make_pair("x", x), std::make_pair("y", y)};
			//Solutions.push_back(Solution{ValuesMap, pow(x,2.1) + 63.9 - 5*y});
		//}
	//}
	

	std::vector<std::string> ValueStrings = {"x"};
	std::vector<Alternative> Alternatives;

	for (int i = 0; i < NumAlternatives; i++)
		Alternatives.push_back(Alternative(ValueStrings,  NumTranslationUnits));

	for (Alternative &A : Alternatives)
		A.Fitness = A.GetFitnessForSolutions(Solutions);

	int count = 0;
	while (true){
		// the main loop

		std::cout << std::endl << std::endl;
		std::cout << "-----------------------------" << std::endl;
		std::cout << "Generation: " << count++ << std::endl;
		std::cout << "Current Best: " << std::endl;
		std::cout << "Tree: " << Alternatives[0].GetTree() << std::endl;
		std::cout << "Fitness: " << Alternatives[0].GetFitnessForSolutions(Solutions) << std::endl;
		NextGeneration(Alternatives, Solutions, ValueStrings);

		std::sort(Alternatives.begin(), Alternatives.end(), [](const Alternative &T1,const Alternative &T2) { return T1.Fitness < T2.Fitness; });
	}
	return 0;
}
