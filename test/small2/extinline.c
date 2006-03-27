//Here is code that might be generated by CIL.  Test that
//when reading it back in, the suffix __extinline doesn't cause problems.

//This bug comes up when merging Apache.

//Make sure the test runs without changes:
//KEEP baseline: success

__inline static int identity__extinline(int x) {
  return x;
}

int foo(int x) {
  return identity__extinline(x);
}

//But what if there's another extern inline function called identity,
// that we want to rename to identity__extinline? 
//Make sure we report an error in this case
inline extern int identity(int x) {  //KEEP bad: error = Trying to rename
  return 10;                         //KEEP bad
}                                    //KEEP bad
int bar(x) { return identity(x); }   //KEEP bad

int identity(int x);
int identity(int x) {
  return x+2;
}


int main(int *p, char** argv)
{
  return identity(-2) + foo(0);
}

