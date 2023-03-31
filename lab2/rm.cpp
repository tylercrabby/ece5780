#include "rm.h"

vector<task> set_priority(vector<task> tasks) {
	sort(tasks.begin(), tasks.end(), compare_task); 
	return tasks;
}

// runs the rate monotonic scheduling algorithm on a set of data then outputs to an output file
void rate_monotonic(int argc, char *argv[]) {
	// get tasks from the file reader
	FileManager fm(argc, argv);
	vector<task> tasks = fm.get_tasks();
	vector<task> a_tasks = fm.get_a_tasks();

	// create a scheduling queue
	queue<task> release_q;

	// declare variables
	task cur_task;
	task prev_task;
	int cur_exe_time_left = 0;

	// run the rate monotonic algorithm
	for (int time = 0; time < fm.get_sim_time(); time++) {
		// check which tasks to release
		release_tasks(&release_q, tasks, time);
		a_tasks = release_a_tasks(&release_q, a_tasks, time);

		// run algorithm
		if (cur_exe_time_left == 0) {
			prev_task = cur_task;
			if (!release_q.empty()) {
				cur_task = release_q.front();
				release_q.pop();
				cur_exe_time_left = cur_task.exe_time - 1;
			}
			if (cur_task.name == prev_task.name) {
				fm.output << time << " idle" << endl;
			}
			else {
				fm.output << time << ": task " << cur_task.name << "\ttime remaining " << cur_exe_time_left << endl;
			}
		}
		else {
			cur_exe_time_left--;
			fm.output << time << ": task " << cur_task.name << "\ttime remaining " << cur_exe_time_left << endl;
		}
	}
}
