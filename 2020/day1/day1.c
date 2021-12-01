#include <stdio.h>
#include <stdlib.h>

int main() {
	FILE *f = fopen("data", "r");
	if (!f) {
		return 1;
	}
	int lines = 0;
	for (;;) {
		char *line = NULL;
		size_t n = 0;
		ssize_t e = getline(&line, &n, f);
		free(line);
		if (e > 0) {
			lines++;
		} else {
			break;
		}
	}

	printf("lines: %d\n", lines);
	int *v = calloc(sizeof *v, lines);

	fseek(f, 0, 0);
	for (int i = 0; i < lines; i++) {
		char *line = NULL;
		size_t n = 0;
		getline(&line, &n, f);
		v[i] = atoi(line);
		free(line);

		for (int j = 0; j < i; j++) {
			int sum2 = v[i] + v[j];
			if (sum2 == 2020) {
				printf("found match: %d and %d\n", v[i], v[j]);
				printf("product: %d\n", v[i] * v[j]);
			} else if (sum2 < 2020) {
				for (int k = 0; k < j; k++) {
					if (v[i] + v[j] + v[k] == 2020) {
						printf("found match: %d and %d and %d\n", v[i], v[j], v[k]);
						printf("product: %d\n", v[i] * v[j] * v[k]);
					}
				}
			}
		}
	}

	fclose(f);
	free(v);
}
