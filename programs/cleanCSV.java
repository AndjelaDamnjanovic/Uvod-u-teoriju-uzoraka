import java.io.*;

public class cleanCSV {
    public static void main(String[] args){
        try(BufferedReader in=new BufferedReader(new InputStreamReader(new FileInputStream("dataset.csv")));
            BufferedWriter out=new BufferedWriter(new OutputStreamWriter(new FileOutputStream("datasetClean.csv")))){
            String line;
            String header;
            header=in.readLine();
            out.write(header);
            out.write("\n");
            while((line=in.readLine())!=null){
                if(!line.equalsIgnoreCase(header)){
                    out.write(line);
                    out.write("\n");
                }
            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
