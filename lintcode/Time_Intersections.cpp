/**
 * Definition of Interval:
 * classs Interval {
 *     int start, end;
 *     Interval(int start, int end) {
 *         this->start = start;
 *         this->end = end;
 *     }
 */
class Solution {
public:
    /**
     * @param seqA: the list of intervals
     * @param seqB: the list of intervals
     * @return: the time periods
     */
    vector<Interval> timeIntersection(vector<Interval> &seqA, vector<Interval> &seqB) {

        auto sortFunc = [](const Interval &i, const Interval &j)->bool{
            return (i.start < j.start || (i.start == j.start && i.end < j.end));
        };

        sort(seqA.begin(),seqA.end(),sortFunc);
        sort(seqB.begin(),seqB.end(),sortFunc);

        vector<Interval> result;

        for(int i = 0,j = 0; i < seqA.size() && j < seqB.size();){
            if(seqA[i].end < seqB[j].start){
                i++;
            }else if(seqB[j].end < seqA[i].start){
                j++;
            }else{
                result.push_back({max(seqA[i].start,seqB[j].start),min(seqA[i].end,seqB[j].end)});

                if(seqA[i].end < seqB[j].end){
                    i++;
                }else{
                    j++;
                }
            }
        }

        return result;
    }
};
