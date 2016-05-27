import java.math.BigDecimal;
public class Problem20 {
    public static void main(String[] args) {
        final BigDecimal zero = new BigDecimal(0);
        final BigDecimal ten = new BigDecimal(10);
        BigDecimal result = new BigDecimal(1);
        for (int i = 1; i < 100; i++) {   
              while (result.remainder(ten).equals(zero)) {
                      result = result.divide(ten);
                }
                result = result.multiply(new BigDecimal(i)); 
        }
        char[] parts = result.toString().toCharArray();
        long acc = 0L;
        for (char num : parts) {
            acc += Character.getNumericValue(num); 
        }
        System.out.println(acc);
    }
}
