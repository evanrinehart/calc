#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct string {
  void* buf;
  size_t len;
};

typedef struct string* string;

string newString(size_t len){
  string str = malloc(sizeof(struct string));
  str->len = len;
  str->buf = malloc(len);
  return str;
}

string newConstantString(const char* data, size_t len){
  string str = newString(len);
  memcpy(str->buf, data, len);
  return str;
}

string catString(string a, string b){
  string c = newString(a->len + b->len);
  memcpy(c->buf, a->buf, a->len);
  memcpy(c->buf+a->len, b->buf, b->len);
  return c;
}


int utf8Unit(unsigned char* buf){
  if(buf[0] < 0x80) return 1;
  if(buf[0] < 0xe0) return 2;
  if(buf[0] < 0xf0) return 3;
  if(buf[0] < 0xf8) return 4;
  fprintf(stderr, "utf8Unit bad utf8\n");
  exit(-1);
}

int utf8Len(struct string* str){
  int i=0;
  int count=0;
  int stride;
  while(i < str->len){
    stride = utf8Unit(str->buf + i);
    count++;
    i += stride;
  }
  return count;
}
