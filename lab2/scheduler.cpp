#include "scheduler.h"

using namespace std;

typedef struct {
	char name;
	int ex_time;
	int period;
} task;

int main(int argc, char *argv[]) {
	ifstream input;
	ofstream output;
	string temp_str;

	input.open(argv[1]);
	output.open(argv[2], ios::out);

	// get number of tasks and simulation time
	int num_tasks;
	int sim_time;	
	task tasks[num_tasks];
	input >> num_tasks;
	input >> sim_time;

	for (int i = 0; i < num_tasks; i++) {
		getline(input, temp_str);
		stringstream parser(temp_str);
		for (int j = 0; j < 3; j++) {
			string s_string;
			getline(parser, s_string, ',');
			cout << s_string << endl;
		}
	}

	output.close();
	return 0;
}
