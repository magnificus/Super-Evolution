#include <string>
#include <stdlib.h>     
#include <time.h>      
#include <cmath>

#pragma once


int TreeDepth = 3;

const float GlobalMutateFactor = 2;

const float MutateChildrenFactor = GlobalMutateFactor * 0.02;
const float MutateFunctionFactor = GlobalMutateFactor * 0.02;
const float MutateLeafFactor = GlobalMutateFactor * 0.02;
const float MutateLeafValFactor = GlobalMutateFactor * 0.02;
const float MutateLeafValFactorMultiply = GlobalMutateFactor * 0.07;
const float MutateLeafSFactor = GlobalMutateFactor * 0.02;

const float MutateStartIndexFactor = GlobalMutateFactor * 0.02;


enum Function {Add,Sub,Mul, Div, Pow, LeftOnly, RightOnly, FUN_END};
enum Leaf {Num,Var, LEAF_END};


// corresponds to a correct solution
struct Solution{
	std::map<std::string, double> InputValues;
	double Result;
};


#define FRAND fRand(0.0, 1.0)

double fRand(double fMin, double fMax)
{
    double f = (double)rand() / RAND_MAX;
    return fMin + f * (fMax - fMin);
}


struct TranslationUnit{
	int ChildL;
	int ChildR;

	Function FunctionType;

	Leaf LeafType;

	double LeafVal;
	std::string LeafS;

	const double GetValue(int Remaining,std::map<std::string, double> &Values,std::vector<TranslationUnit> &TranslationUnits) {
		if (Remaining == 0)
			return GetLeafValue(Values);

		double Val1 = TranslationUnits[ChildL].GetValue(Remaining-1, Values, TranslationUnits);
		double Val2 = TranslationUnits[ChildR].GetValue(Remaining-1, Values, TranslationUnits);

		switch (FunctionType){
			case Add: return Val1 + Val2;
			case Sub: return Val1 - Val2;
			case Mul: return Val1 * Val2;
			case Div: return Val1 / (Val2 == 0 ? 1 : Val2);
			case Pow: return pow(Val1,Val2);
			case LeftOnly: return Val1;
			case RightOnly: return Val2;
		}

		return -100000;
	}

	// mostly for debugging
	const std::string GetTree (int Remaining, std::vector<TranslationUnit> &TranslationUnits) {
		if (Remaining == 0){
			switch (LeafType) {
				case Num: return std::to_string(LeafVal);
				case Var: return /*std::to_string(LeafVal) + */LeafS;
			}
			return "INVALID";
		} else {
			std::string String1 = TranslationUnits[ChildL].GetTree(Remaining-1, TranslationUnits);
			std::string String2 = TranslationUnits[ChildR].GetTree(Remaining-1, TranslationUnits);

			std::string FuncS;

			switch (FunctionType){
				case Add: FuncS = "+"; break;
				case Sub: FuncS = "-"; break;
				case Mul: FuncS = "*"; break;
				case Div: FuncS = "/"; break;
				case Pow: FuncS = "^"; break;
				case LeftOnly: return String1;
				case RightOnly: return String2;
				default: FuncS = "INVALID"; break;
			}
			return "(" + String1 + " " + FuncS + " " + String2 + ")";

		}
	}

	const double GetLeafValue(std::map<std::string, double> &Values){
		switch (LeafType){
			case Num: return LeafVal;
			case Var: return Values[LeafS]/**LeafVal*/;
		}
		return -10000000;
	}


	static TranslationUnit GetRandomTranslationUnit(const std::vector<std::string> &Values, int NumTranslationUnits){

		int NewChildL = rand() % NumTranslationUnits;
		int NewChildR = rand() % NumTranslationUnits;

		Function NewFunctionType = static_cast<Function>(rand() % FUN_END);
		Leaf NewLeafType = static_cast<Leaf>(rand() % LEAF_END);

		double NewLeafVal = fRand(-1.0, 1.0);
		std::string NewLeafS = Values[rand() % Values.size()];

		return TranslationUnit{NewChildL, NewChildR, NewFunctionType, NewLeafType, NewLeafVal, NewLeafS}; 
	}

	void Mutate(std::vector<std::string> &ValueStrings, int NumTranslationUnits){
		if (FRAND < MutateChildrenFactor)
			ChildL = rand() % NumTranslationUnits;

		if (FRAND < MutateChildrenFactor)
			ChildR = rand() % NumTranslationUnits;

		if (FRAND < MutateFunctionFactor)
			FunctionType = static_cast<Function>(rand() % FUN_END);

		if (FRAND < MutateLeafFactor)
			LeafType = static_cast<Leaf>(rand() % LEAF_END);

		if (FRAND < MutateLeafValFactor)
			LeafVal = fRand(-1.0,1.0);

		if (FRAND < MutateLeafValFactorMultiply)
			LeafVal *= pow(fRand(0.65,1.5), 10);

		if (FRAND < MutateLeafSFactor)
			LeafS = ValueStrings[rand() % ValueStrings.size()];

	}

};

struct Alternative{
	std::vector<TranslationUnit> TranslationUnits;
	int StartIndex = 0;
	double Fitness = -1.0;

	Alternative(std::vector<std::string> &ValueStrings, int NumUnits){
		StartIndex = rand() % NumUnits;
		for (int i = 0; i < NumUnits; i++)
			TranslationUnits.push_back(TranslationUnit::GetRandomTranslationUnit(ValueStrings, NumUnits));
	}

	Alternative(std::vector<TranslationUnit> InUnits, int InStart) : TranslationUnits(InUnits), StartIndex(InStart) {};


	const double GetValue(std::map<std::string, double> &Values){
		return TranslationUnits[StartIndex].GetValue(TreeDepth, Values, TranslationUnits);
	}


	const double GetFitnessForSolution(Solution &InputSol){
		double MyResult = GetValue(InputSol.InputValues);
		return pow(abs(MyResult - InputSol.Result), 2.0);
	}

	const double GetFitnessForSolutions(std::vector<Solution> &Solutions){
	
		double Total = 0.0;
		for (Solution &S : Solutions)
			Total += GetFitnessForSolution(S);
		return Total;
	}
	void Mutate(std::vector<std::string> &ValueStrings){
		if (FRAND < MutateStartIndexFactor)
			StartIndex = rand() % TranslationUnits.size();


		for (TranslationUnit &T : TranslationUnits){
			T.Mutate(ValueStrings, TranslationUnits.size());
		}
	}

	std::string GetTree(){
		return TranslationUnits[StartIndex].GetTree(TreeDepth, TranslationUnits);
	}

	static Alternative GetChild(Alternative &P1, Alternative &P2){
		int NumTU = P1.TranslationUnits.size();

		std::vector<TranslationUnit> NewUnits;

		for (int i = 0; i < NumTU; i++){
			NewUnits.push_back(FRAND < 0.5 ? P1.TranslationUnits[i] : P2.TranslationUnits[i]);
		}

		int NewStart = FRAND < 0.5 ? P1.StartIndex : P2.StartIndex; 
		return Alternative(NewUnits, NewStart);
		
	}
};


