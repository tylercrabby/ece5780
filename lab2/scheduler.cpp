#include "scheduler.h"

using namespace std;

int main() {
	ifstream input;
	input.open("input.txt");
	string temp_str;

	while (input) {
		getline(input, temp_str);
		cout << temp_str << endl;
	}	
}
