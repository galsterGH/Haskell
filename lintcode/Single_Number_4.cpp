class Solution {
public:
    /**
     * @param nums: The number array
     * @return: Return the single number
     */
    int getSingleNumber(vector<int> &nums) {
        //8  8 9 9 10 10 11 11 12 12 13 13 14 14 15 16 16

        int start = 0;
        for(int end = nums.size();start < end;){
                 int med = (start + end)/2;

                 if(med > start && nums[med] == nums[med - 1]){
                     if((med - start) % 2){
                         start = med + 1;
                     }
                     else{
                         end = med - 1;
                     }
                 }else if (med < end - 1 && nums[med] == nums[med + 1]){
                     if((med + 1 - start)%2){
                         start = med + 2;
                     }else{
                         end = med;
                     }
                 }
                 else{
                    return nums[med];
                 }
             }

        return nums[start];
    }
};
