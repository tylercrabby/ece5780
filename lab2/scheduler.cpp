#include "scheduler.h"

int main(int argc, char *argv[]) {
	ifstream input;
	ofstream output;

	input.open(argv[1]);
	output.open(argv[2], ios::out);

	// get number of tasks and simulation time
	int num_tasks;
	int num_a_tasks;
	int sim_time;	
	string temp_str;
	vector<string> lines;
	vector<task> tasks;
	vector<task> a_tasks;
	input >> num_tasks;
	input >> sim_time;
	
	getline(input, temp_str);

	for (int i = 0; i < num_tasks; i++) {
		getline(input, temp_str);
		lines.push_back(temp_str);
	}

	for (int i = 0; i < num_tasks; i++) {
		stringstream ss(lines[i]);
		task temp_t;
		char comma;

		ss >> temp_t.name >> comma >> temp_t.ex_time >> comma >> temp_t.period;
		tasks.push_back(temp_t);
	}

	for (int i = 0; i < num_tasks; i++) {
		cout << tasks[i].name << ' ' << tasks[i].ex_time << ' ' << tasks[i].period << endl;
	}


	// get number of asyncronous tasks
	input >> num_a_tasks;

	getline(input, temp_str);
	lines.clear();

	for (int i = 0; i < num_a_tasks; i++) {
		getline(input, temp_str);
		lines.push_back(temp_str);
	}
	
	for (int i = 0; i < num_a_tasks; i++) {
		stringstream ss(lines[i]);
		task temp_t;
		char comma;

		ss >> temp_t.name >> comma >> temp_t.ex_time >> comma >> temp_t.period;
		a_tasks.push_back(temp_t);
	}
	
	for (int i = 0; i < num_a_tasks; i++) {
		cout << a_tasks[i].name << ' ' << a_tasks[i].ex_time << ' ' << tasks[i].period << endl;
	}

	output.close();
	return 0;
}
