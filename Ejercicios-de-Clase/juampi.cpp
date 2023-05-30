
#include <iostream>
#include <stdio.h>
#include <math.h>
#include <string.h>

using namespace std;

#include <iostream>

using namespace std;

int indexOf(string s, string toSearch);

int main()
{
    string s = "Esto euns una prueba", toSearch = "una";
    int p = indexOf(s, toSearch);
    cout << p << endl;

    p = indexOf(s, "jamon");
    cout << p << endl;

    return 0;
}

int indexOf(string s, string toSearch)
{
    int lS = s.length();
    int lTS = toSearch.length();
    int i = 0,
        j = 0;
    int position = -1;

    // string s = "Esto euns una prueba", toSearch = "una";
    while (i < lS && j < lTS)
    {
        if (s[i] == toSearch[j])
        {
            if (position == -1)
            {
                position = i;
            }
            i++;
            j++;
        }
        else
        {
            position = -1;
            i++;
            j = 0;
        }
    }

    return position;
}
