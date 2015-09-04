#include <npth.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void print(int *value) 
{
  printf("Thread %d\n", *value);
}

int main(int argc, char** argv) 
{
  npth_attr_t t_attr1;
  npth_t thread1;
  int i;
  npth_init ();
  for(i = 0; i < 5; i++) {
    npth_attr_init(&t_attr1);
    // N.B.: value i will be always 5 because npth is cooperative, and &i is 
    //       stored in the main thread. (all threads created is  cooperative)
    npth_create(&thread1, &t_attr1, (void *)&print, (void *)&i);
  }
  npth_exit(0);

  return 0;
}
