#include <iostream>
using namespace std;
//3
struct SList
{
    char** _words;
    size_t _len;

    SList(char * s)
    {
      _words = split(s);
    }

    char** split(char * str)
    {
      //TODO splitting string by whitespaces. Incrementing _len on every iteration
    }

    char* operator[](size_t i)
    {
      return _words[i];
    }

    //5
    char* toString() // a member function of SList
    {
      size_t length(0);
      for (size_t i(0); i < _len; i++)
      {
        for (size_t j(0); j < strlen(_words[i]); j++)
        {
          ++length;
        }
      }

      char* to_s = new char[length];

      size_t idx(0);

      for (size_t i(0); i < _length; i++)
      {
        for (size_t j(0); j < strlen(_words[i]); j++)
        {
          to_s[idx] = words[i][j];
          ++idx;
        }
        to_s[idx] = ' ';
      }
      return to_s;
    }
}

//4
ostream operator<<(ostream& out, SList s)
{
  for (size_t i = 0; i < s._len; i++)
  {
    out << s[i] << ' ';
  }
  return out;
}
