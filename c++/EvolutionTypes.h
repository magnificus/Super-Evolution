#include<string>
#include <stdlib.h>     
#include <time.h>      

#pragma once


enum Function {Add,Sub,Mul, FUN_END};
enum Leaf {Num,Var, LEAF_END};


// corresponds to a correct solution
struct Solution{
	std::map<std::string, double> InputValues;
	double Result;
}

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


	// mostly for debugging
	std::string GetTree (int Remaining, std::vector<TranslationUnit> &TranslationUnits) {
		if (Remaining == 0){
			switch (LeafType) {
				case Num: return std::to_string(LeafVal);
				case Var: return LeafS;
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
				default: FuncS = "INVALID"; break;
			}
			return "(" + String1 + " " + FuncS + " " + String2 + ")";

		}
	}

	double GetValue(int Remaining,std::map<std::string, double> &Values,std::vector<TranslationUnit> &TranslationUnits) {
		if (Remaining == 0)
			return GetLeafValue(Values);

		double Val1 = TranslationUnits[ChildL].GetValue(Remaining-1, Values, TranslationUnits);
		double Val2 = TranslationUnits[ChildR].GetValue(Remaining-1, Values, TranslationUnits);

		switch (FunctionType){
			case Add: return Val1 + Val2;
			case Sub: return Val1 - Val2;
			case Mul: return Val1 * Val2;
		}

		return 0.0f;
	}

	double GetLeafValue(std::map<std::string, double> &Values){
		switch (LeafType){
			case Num: return LeafVal;
			case Var: return Values[LeafS];
		}
		return 0.0f;
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

};



