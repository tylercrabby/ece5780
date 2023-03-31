#include "task_manager.h"

bool compare_task(const task t1, const task t2) {
	return t1.period < t2.period;
}

bool is_task_equal(const task t1, const task t2) {
	return t1.name == t2.name;
}

//release_tasks checks the queue of tasks to see which of the tasks needs to be processed first.
void release_tasks(queue<task>* q, vector<task> tasks, int time) {
	if (time == 0) {
		for (int i = 0; i < tasks.size(); i++) {
			tasks[i].task_state = tasks[i].period - 1;
			q->push(tasks[i]);
			cout << "Task " << tasks[i].name << " was pushed at time " << time << "." << endl;
		}
	}
	else {
		for (int i = 0; i < tasks.size(); i++) {
			if (time % tasks[i].period == 0) {
				tasks[i].task_state = time + tasks[i].period - 1;
				q->push(tasks[i]);
				cout << "task pushed: " << time << " " << tasks[i].period << " " << time % tasks[i].period << endl;
			}
		}
	}
}

vector<task> release_a_tasks(queue<task>* q, vector<task> tasks, int time) {
	if (time != 0) {
		for (int i = 0; i < tasks.size(); i++) {
			if (time % tasks[i].period == 0) {
				q->push(tasks[i]);
				tasks.erase(tasks.begin() + i);
				cout << "a task pushed: " << time << " " << tasks[i].period << " " << time % tasks[i].period << endl; 
			}
		}
	}
	return tasks;
}
