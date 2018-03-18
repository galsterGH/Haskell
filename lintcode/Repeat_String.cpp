class Solution {
public:
    /**
     * @param A: string A to be repeated
     * @param B: string B
     * @return: the minimum number of times A has to be repeated
     */
    int repeatedString(string &A, string &B) {

        if(A.size() == 0){
            return -1;
        }

        if(B.size() <= A.size()){
            if(A.find(B) == string::npos){
                return -1;
            }

            return 1;
        }

        vector<int> kmp(B.size(),0);

        for(int i = 0,j = 0; i < B.size();){
            if(B[i] == A[j%(A.size())]){
                kmp[i] = j + 1;
                j++;
                i++;
            }else{
                if(j == 0){
                    i++;
                }else{
                    j = 0;
                }
            }
        }

        auto diff = kmp.size() - kmp.back();

        if(diff > A.size() || A.find(B.substr(0,diff)) == string::npos || kmp.back() < A.size()){
            return -1;
        }

        return (kmp.back() + A.size() - 1)/A.size()  + (diff != 0 ? 1 : 0);
    }
};
