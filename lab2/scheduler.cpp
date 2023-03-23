/*
Carter Nettesheim
Tyler Crabb
Lab 2 ~ Rate Monotonic and Earliest Deadline First Scheduling

23MAR2023
Dr. Phillips' ECE5780 Embedded Real-Time Systems
*/

#include "scheduler.h"

/*
This big bad scheduling program takes an input file of tasks, their runtimes, and release windows and creates a schedule to run them all by.
The schedules will be written to an output text file that the user will name.
*/
int main(int argc, char *argv[]) {
	// We need files to operate on.
	ifstream input;
	ofstream output;

	// Open and create files named by the user to get our scheduling constraints and write the schedule to.
	input.open(argv[1]);
	output.open(argv[2], ios::out);

	// Fetch the number of tasks, their names, their simulation time, and release times (in milliseconds).
	int num_tasks;
	int num_a_tasks;
	int sim_time;
	
	string temp_str;			// A temporary spot to save the task data before it gets moved to the task struct.
	
	vector<string> lines;			// Build an array of strings to store the task data in.
	vector<task> tasks;			// Build an array of task structs to store task data in.
	vector<task> a_tasks;			// Build an array of asynchronous task structs to store the asyncronous task data in.
	input >> num_tasks;			// How many total tasks are we working with?
	input >> sim_time;			// How many milliseconds do we have to build a schedule with?
	
	getline(input, temp_str);

	// Step through the input file's tasks and read their names, durations, and release data.
	for (int i = 0; i < num_tasks; i++) {
		getline(input, temp_str);
		lines.push_back(temp_str);
	}

	for (int i = 0; i < num_tasks; i++) {
		stringstream ss(lines[i]);	// Load the task data from the queue.
		task temp_t;			// This temp_t task struct will be the frame to store the data in.
		char comma;

		// Take this task's data and load it piece-wise into the task struct.
		ss >> temp_t.name >> comma >> temp_t.ex_time >> comma >> temp_t.period;
		tasks.push_back(temp_t);	// Back the the queue with you.
	}

	// Step through the tasks and show their data to the console.
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
