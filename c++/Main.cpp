#include<iostream>
#include<vector>
#include<map>
#include<functional>
#include<algorithm>
#include "EvolutionTypes.h"

int NumTranslationUnits = 200;
int NumAlternatives = 1000;

float KeepRatio = 0.85;

std::vector<Solution> GetCustomSolutions(){


	// edit these, but keep the number of entries the same in each array
	double XVals[] = {1.0,2.0,3.0, 4.0};
	double YVals[] = {2.0,4.0, 6.0, 0.0};
	double ResultVals[] = {1+4,2+8,3+12,4};

	std::vector<Solution> ToReturn;
	// only x
	//for (int i = 0; i < sizeof(XVals)/sizeof(XVals[0]); i++){
	//	std::map<std::string, double> Current = {std::make_pair("x", XVals[i])};
	//	ToReturn.push_back(Solution{Current, ResultVals[i]});
	//}
	// x and y assume same length of arrays 
	for (int i = 0; i < sizeof(XVals)/sizeof(XVals[0]); i++){
		std::map<std::string, double> Current = {std::make_pair("x", XVals[i]), std::make_pair("y", YVals[i])};
		ToReturn.push_back(Solution{Current, ResultVals[i]});
	}

	return ToReturn;

}


void NextGeneration(std::vector<Alternative> &Alternatives, std::vector<Solution> &Solutions, std::vector<std::string> &ValueStrings){

	#pragma omp parallel for
	for (int i = 0; i < Alternatives.size(); i++){
		bool ShouldCull = KeepRatio < FRAND*2.0*(float(i) / Alternatives.size());
		if (ShouldCull){
			int Parent1 = rand() % Alternatives.size();
			int Parent2 = rand() % Alternatives.size();

			//Alternatives[i] = Alternatives[Parent1];
			Alternatives[i] = Alternative::GetChild(Alternatives[Parent1], Alternatives[Parent2]);
			Alternatives[i].Mutate(ValueStrings);
			Alternatives[i].Fitness = Alternatives[i].GetFitnessForSolutions(Solutions);
		}
	}
}

int main(){

	srand (time(NULL));

	std::vector<Solution> Solutions;


	double MinInputValue = 0.0;
	double MaxInputValue = 12.0;

	// only x
	//for (double x = MinInputValue; x < MaxInputValue; x++){
	//	std::map<std::string, double> ValuesMap = {std::make_pair("x", x)};
	//	Solutions.push_back(Solution{ValuesMap, pow(x,2.1) });
	//}

	// x and y
	//for (double x = MinInputValue; x < MaxInputValue; x++){
	//	for (double y = MinInputValue; y < MaxInputValue; y++){
	//		std::map<std::string, double> ValuesMap = {std::make_pair("x", x), std::make_pair("y", y)};
	//		Solutions.push_back(Solution{ValuesMap, x+y-2);
	//	}
	//}
	
	Solutions = GetCustomSolutions();
	

	std::vector<std::string> ValueStrings;
	if (Solutions.size() > 0){
		for (auto pair : Solutions[0].InputValues){
			ValueStrings.push_back(std::get<0>(pair));
		}
	}

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

		// sort by fitness
		std::sort(Alternatives.begin(), Alternatives.end(), [](const Alternative &T1,const Alternative &T2) { return T1.Fitness < T2.Fitness; });
	}
	return 0;
}
