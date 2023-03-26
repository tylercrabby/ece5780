/*
Carter Nettesheim
Tyler Crabb
Lab 2 ~ Rate Monotonic and Earliest Deadline First Scheduling
23MAR2023
Dr. Phillips' ECE5780 Embedded Real-Time Systems
*/
#include "scheduler.h"


void release_tasks(queue<task>* q, vector<task> tasks, int time) {
	if (time = 0) {
		for (int i = 0; i < tasks.size(); i++) {
			q->push(tasks[i]);
		}
	}
	else {
		for (int i = 0; i < tasks.size(); i++) {
			if (time % tasks[i].period == 0) {
				q->push(tasks[i]);
			}
		}
	}
}

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
	int time;
	
	string temp_str;			// A temporary spot to save the task data before it gets moved to the task struct.
	
	vector<string> lines;			// Build an array of strings to store the task data in.
	vector<task> tasks;			// Build an array of task structs to store task data in.
	vector<task> a_tasks;			// Build an array of asynchronous task structs to store the asyncronous task data in.
	queue<task> release_q;
	task cur_task;
	int cur_exe_time_left = 0;
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

		ss >> temp_t.name >> comma >> temp_t.exe_time >> comma >> temp_t.period;
		tasks.push_back(temp_t);	// Back of the queue with you.
	}

	// Step through the tasks and show their data to the console.
	for (int i = 0; i < num_tasks; i++) {
		cout << tasks[i].name << ' ' << tasks[i].exe_time << ' ' << tasks[i].period << endl;
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

		ss >> temp_t.name >> comma >> temp_t.exe_time >> comma >> temp_t.period;
		a_tasks.push_back(temp_t);
	}
	
	for (int i = 0; i < num_a_tasks; i++) {
		cout << a_tasks[i].name << ' ' << a_tasks[i].exe_time << ' ' << tasks[i].period << endl;
	}

	// rate monotonic
	for (time = 0; time < sim_time; time++) {
		//check which tasks to release
		release_tasks(&release_q, tasks, time);
		//execute first task in the release queue
		if (cur_exe_time_left == 0) {
			if (!release_q.empty()) {
				cur_task = release_q.front();
				release_q.pop();
				cur_exe_time_left = cur_task.exe_time - 1;
			}
			output << time << ": task " << cur_task.name << "\ttime remaining " << cur_exe_time_left << endl;
		}
		else {
			cur_exe_time_left--;
			output << time << ": task " << cur_task.name << "\ttime remaining " << cur_exe_time_left << endl;
		}	
	}

	output.close();
	return 0;
}
