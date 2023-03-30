#ifndef TASK_MANAGER_H
#define TASK_MANAGER_H
#include <stdio.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <queue>

using namespace std;

typedef struct {
	char name;
	int exe_time;
	int period;

	// We need a variable to track which state this task has been in.
	int task_state = 0;
} task;

const int IMPLICIT_DEADLINE = 500;

void release_tasks(queue<task>* q, vector<task> tasks, int time);
vector<task> release_a_tasks(queue<task>* q, vector<task> tasks, int time);

#endif
