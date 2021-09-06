#include <iostream>
#include <vector>
#include <algorithm>
#define numtype unsigned long long

using namespace std;


const string ALP = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz .";

numtype char_to_int(char ch) {
    if (ch >= '0' && ch <= '9')
        return int(ch - '0');
    if (ch >= 'A' && ch <= 'Z')
        return int(ch - 'A') + 10;
    if (ch >= 'a' && ch <= 'z')
        return int(ch - 'a') + 10 + 26;
    if (ch == ' ')
        return 62;
    if (ch == '.')
        return 63;
    return 64;
}
char int_to_char(numtype dig) {
    if (0 <= dig && dig <= 9) {
        return char(int('0') + dig);
    }
    if (10 <= dig && dig <= 35) {
        return char(int('A') + dig - 10);
    }
    if (36 <= dig && dig <= 61) {
        return char(int('a') + dig - 36);
    }
    if (dig == 62) {
        return ' ';
    }
    return '.';
}


numtype p;

numtype pow(numtype a, numtype b) {
    if (b == 0) {
        return 1;
    }
    if (b == 1) {
        return a % p;
    }
    if (b % 2 == 0) {
        return pow((a * a) % p, b / 2);
    }
    else {
        return (a * pow((a * a) % p, b / 2)) % p;
    }
}

numtype inv(numtype a) {
    return pow(a, p - 2);
}

numtype dividemodp(numtype a, numtype b) {
    return (a * inv(b)) % p;
}

class Poly {
public:
    numtype sz = 0;
    vector<numtype> a;

    void delZero() {
        while (!a.empty() && a[a.size() - 1] == 0) {
            a.pop_back();
        }
        if (a.empty()) {
            a.push_back(0);
        }
        sz = a.size();
    }

    Poly() {
        sz = 1;
        a.push_back(0);
    }
    Poly(numtype sz1) {
        sz = sz1;
        a.resize(sz1);
    }
    Poly(vector<numtype> &v) {
        a = v;
        delZero();
    }
    Poly (const Poly& other){
        this -> sz = other.sz;
        this -> a = other.a;
    }

    friend bool operator == (Poly &p1, Poly &p2) {
        p1.delZero();
        p2.delZero();
        if (p1.sz != p2.sz) {
            return false;
        }
        for (numtype i = 0; i < p1.sz; ++i) {
            if (p1.a[i] != p2.a[i]) {
                return false;
            }
        }
        return true;
    }
    friend bool operator != (Poly &p1, Poly &p2) {
        return !(p1 == p2);
    }
    friend bool operator != (Poly p1, Poly p2) {
        return !(p1 == p2);
    }
    friend bool operator < (Poly &p1, Poly &p2) {
        p1.delZero();
        p2.delZero();
        if (p1.sz != p2.sz) {
            return p1.sz < p2.sz;
        }
        for (numtype i = p1.sz - 1; i >= 0; --i) {
            if (p1.a[i] != p2.a[i]) {
                return p1.a[i] < p2.a[i];
            }
        }
        return false;
    }
    friend bool operator <= (Poly &p1, Poly &p2) {
        return (p1 == p2) || (p1 < p2);
    }
    friend bool operator > (Poly &p1, Poly &p2) {
        return p2 < p1;
    }
    friend bool operator >= (Poly &p1, Poly &p2) {
        return (p1 == p2) || (p1 > p2);
    }

    friend Poly operator * (Poly &p1, Poly &p2) {
        p1.delZero();
        p2.delZero();
        Poly res = Poly(p1.sz + p2.sz - 1);
        for (numtype i = 0; i < p1.sz; ++i) {
            for (numtype j = 0; j < p2.sz; ++j) {
                res.a[i + j] = (res.a[i + j] + (p1.a[i] * p2.a[j]) % p) % p;
            }
        }
        res.delZero();
        return res;
    }
    friend Poly operator * (Poly &p1, numtype alpha) {
        p1.delZero();
        Poly res = Poly(p1.a);
        for (numtype i = 0; i < res.sz; ++i) {
            res.a[i] = (res.a[i] * alpha) % p;
        }
        res.delZero();
        return res;
    }
    friend Poly operator + (Poly &p1, Poly &p2) {
        p1.delZero();
        p2.delZero();
        Poly res = Poly(max(p1.sz, p2.sz));
        for (numtype i = 0; i < p1.sz; ++i) {
            res.a[i] = p1.a[i];
        }
        for (numtype i = 0; i < p2.sz; ++i) {
            res.a[i] = (res.a[i] + p2.a[i]) % p;
        }
        //res.delZero();
        return res;
    }
    friend Poly operator - (Poly &p1, Poly &p2) {
        p1.delZero();
        p2.delZero();
        Poly res = Poly(max(p1.sz, p2.sz));
        for (numtype i = 0; i < p1.sz; ++i) {
            res.a[i] = p1.a[i];
        }
        for (numtype i = 0; i < p2.sz; ++i) {
            res.a[i] = (res.a[i] - p2.a[i] + p) % p;
        }
        res.delZero();
        return res;
    }

    friend Poly operator % (Poly &p1, Poly &f) {
        p1.delZero();
        f.delZero();
        Poly cur = Poly(p1.sz);
        if (p1.sz < f.sz) {
            cur = p1;
            return cur;
        }

        Poly p0 = p1 / f;
        p0 = p0 * f;
        cur = p1 - p0;
        cur.delZero();
        return cur;
    }
    friend Poly operator / (Poly &p1, Poly &f) {
        p1.delZero();
        f.delZero();
        Poly cur = p1;
        Poly res = Poly(p1.sz);
        if (p1.sz < f.sz) {
            res = Poly(1);
            return res;
        }

        numtype alpha;
        Poly cur0;
        while (cur >= f) {
            alpha = dividemodp(cur.a[cur.sz - 1], f.a[f.sz - 1]);
            res.a[cur.sz - f.sz] = alpha;
            cur0 = (f * alpha).shift(cur.sz - f.sz);
            cur = cur - cur0;
        }
        res.delZero();
        return res;
    }


    bool isZero() {
        for (numtype i = 0; i < sz; ++i) {
            if (a[i] != 0) {
                return false;
            }
        }
        return true;
    }
    Poly shift(numtype delta) {
        Poly res = Poly(this -> a);
        if (delta != 0) {
            res.sz += delta;
            res.a.resize(a.size() + delta);
            for (numtype i = res.sz - 1; i >= delta; --i) {
                res.a[i] = res.a[i - delta];
            }
            for (numtype i = delta - 1; i > 0; --i) {
                res.a[i] = 0;
            }
            res.a[0] = 0;
        }
        return res;
    }

    friend std::ostream& operator << (std::ostream& out, const Poly &p1){
        for (numtype i = 0; i < p1.sz; ++i) {
            out << p1.a[i] << " ";
        }
        return out;
    }
};

void string_to_vector(string &s, vector<numtype> &res) {
    numtype cur10 = 0;
    bool neg = false;
    for (numtype i = 0; i < (numtype)s.size(); ++i) {
        if (s[i] == '-') {
            neg = true;
        }
        else if (s[i] == ' ') {
            if (neg) {
                res.push_back(p - cur10);
            }
            else {
                res.push_back(cur10);
            }
            cur10 = 0;
            neg = false;
        }
        else {
            cur10 *= 10;
            cur10 += (s[i] - '0');
        }
    }
    if (neg) {
        res.push_back(p - cur10);
    }
    else {
        res.push_back(cur10);
    }
}


numtype modulep(string &s, numtype p) {
    numtype ans = 0;
    for (numtype i = 0; i < (numtype)s.size(); ++i) {
        ans *= 64;
        ans += char_to_int(s[i]);
        ans %= p;
    }
    return ans;
}

void dividep(string &s, numtype p) {
    numtype i = 0;
    numtype cur10 = 0;
    string res = "";
    while (i < (numtype)s.size()) {
        cur10 *= 64;
        cur10 += char_to_int(s[i]);
        if (cur10 >= p) {
            res += int_to_char(cur10 / p);
            cur10 = cur10 % p;
        }
        else {
            if (res.size() > 0) {
                res += '0';
            }
        }
        i++;
    }
    if (res == "") {
        res = "0";
    }
    s = res;
}

void conv_64_to_p(string &s, numtype p, vector<numtype> &ans) {
    while (s != "0") {
        ans.push_back(modulep(s, p));
        dividep(s, p);
    }
}

vector<Poly> split(vector<numtype> &messagep, numtype n) {
    vector<Poly> res;
    for (numtype i = 0; i < (numtype)messagep.size(); i += n) {
        vector<numtype> resi;
        for (numtype j = 0; j < n; ++j) {
            if (i + j < (numtype)messagep.size()) {
                resi.push_back(messagep[i + j]);
            }
        }
        res.push_back(Poly(resi));
    }
    return res;
}


int main() {

    cin >> p;
    ws(cin);

    string inp;

    getline(cin, inp);
    vector<numtype> finp;
    string_to_vector(inp, finp);
    Poly f = Poly(finp);

    getline(cin, inp);
    vector<numtype> ginp;
    string_to_vector(inp, ginp);
    Poly g = Poly(ginp);

    getline(cin, inp);
    vector<numtype> yinp;
    string_to_vector(inp, yinp);
    Poly y = Poly(yinp);

    string message64;
    getline(cin, message64);
    reverse(message64.begin(), message64.end());
    vector<numtype> messagep;
    conv_64_to_p(message64, p, messagep);

    vector<Poly> messagep_splitted = split(messagep, f.sz - 1);

    for (Poly &M : messagep_splitted) {
        Poly gk = (g * g);
        gk = gk % f;
        Poly yk = (y * y);
        yk = yk % f;
        Poly b = (yk * M);
        b = b % f;
        cout << gk << '\n';
        cout << b << '\n';
    }
    return 0;

}
