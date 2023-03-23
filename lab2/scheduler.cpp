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

int main(int argc, char *argv[]) {
	ifstream input;
	ofstream output;

	input.open(argv[1]);
	output.open(argv[2], ios::out);

	// get number of tasks and simulation time
	int num_tasks;
	int num_a_tasks;
	int sim_time;	
	int time;
	string temp_str;
	vector<string> lines;
	vector<task> tasks;
	vector<task> a_tasks;
	queue<task> release_q;
	task cur_task;
	int cur_exe_time_left = 0;
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

		ss >> temp_t.name >> comma >> temp_t.exe_time >> comma >> temp_t.period;
		tasks.push_back(temp_t);
	}

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
