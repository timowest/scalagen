package com.mysema.examples;

public class ParallelColt {
    
    public static class Axis {

        public Object binLowerEdge(int i) {
            // TODO Auto-generated method stub
            return null;
        }
        

        public int bins() {
            // TODO Auto-generated method stub
            return 0;
        }

        public Object lowerEdge() {
            // TODO Auto-generated method stub
            return null;
        }


        public Object upperEdge() {
            // TODO Auto-generated method stub
            return null;
        }
        
    }
    
    public static class DoubleBinFunction1D {
        
    }

    public static class DoubleFormatter {

        public String toTitleString(DoubleMatrix2D heights, String[] yEdges,
                String[] xEdges, String rowAxisName, String columnAxisName,
                Object object, DoubleBinFunction1D[] aggr) {
            // TODO Auto-generated method stub
            return null;
        }
        
    }
    
    public static class DoubleIHistogram2D {

        public Object binHeight(int i, int j) {
            // TODO Auto-generated method stub
            return null;
        }

        public Object entries() {
            // TODO Auto-generated method stub
            return null;
        }

        public Object extraEntries() {
            // TODO Auto-generated method stub
            return null;
        }

        public Object meanX() {
            // TODO Auto-generated method stub
            return null;
        }

        public Object meanY() {
            // TODO Auto-generated method stub
            return null;
        }

        public int[] minMaxBins() {
            // TODO Auto-generated method stub
            return null;
        }
        
        public Object rmsX() {
            // TODO Auto-generated method stub
            return null;
        }

        public String title() {
            // TODO Auto-generated method stub
            return null;
        }

        public Axis xAxis() {
            // TODO Auto-generated method stub
            return null;
        }

        public Axis yAxis() {
            // TODO Auto-generated method stub
            return null;
        }
        
    }
    
    public static class DoubleMatrix2D {

        public Object viewDice() {
            // TODO Auto-generated method stub
            return null;
        }
        
    }
    
    public static class Former {
        
    }
    
    public static class FormerFactory {

        public Former create(String format) {
            // TODO Auto-generated method stub
            return null;
        }
        
    }
    
    public static DoubleBinFunction1D sum = null;
    
    private String form(Former f, Object extraEntries) {
        // TODO Auto-generated method stub
        return null;
    }

    public String toString(DoubleIHistogram2D h) {
        String columnAxisName = "X";
        String rowAxisName = "Y";
        DoubleBinFunction1D[] aggr = { sum };
        String format = "%G";
        // String format = "%1.2G";

        Former f = new FormerFactory().create(format);
        String sep = System.getProperty("line.separator");
        int[] minMaxBins = h.minMaxBins();
        String title = h.title() + ":" + sep + "   Entries=" + form(f, h.entries()) + ", ExtraEntries="
//                + form(f, h.extraEntries()) + sep + "   MeanX=" + form(f, h.meanX()) + ", RmsX=" + form(f, h.rmsX())
//                + sep + "   MeanY=" + form(f, h.meanY()) + ", RmsY=" + form(f, h.rmsX()) + sep + "   MinBinHeight="
//                + form(f, h.binHeight(minMaxBins[0], minMaxBins[1])) + ", MaxBinHeight="
//                + form(f, h.binHeight(minMaxBins[2], minMaxBins[3])) + sep +
//
//                "   xAxis: " + "Bins=" + form(f, h.xAxis().bins()) + ", Min=" + form(f, h.xAxis().lowerEdge())
//                + ", Max=" + form(f, h.xAxis().upperEdge()) + sep +
//
//                "   yAxis: " + "Bins=" + form(f, h.yAxis().bins()) + ", Min=" + form(f, h.yAxis().lowerEdge())
                + ", Max=" + form(f, h.yAxis().upperEdge());
        
        String[] xEdges = new String[h.xAxis().bins()];
        for (int i = 0; i < h.xAxis().bins(); i++)
            xEdges[i] = form(f, h.xAxis().binLowerEdge(i));

        String[] yEdges = new String[h.yAxis().bins()];
        for (int i = 0; i < h.yAxis().bins(); i++)
            yEdges[i] = form(f, h.yAxis().binLowerEdge(i));
//        new ObjectArrayList(yEdges).reverse(); // keep coord.
        // system

//        DoubleMatrix2D heights = new DenseDoubleMatrix2D(toArrayHeights(h));
//        heights = heights.viewDice().viewRowFlip(); // keep the histo coord.
        // system
        // heights = heights.viewPart(1,1,heights.rows()-2,heights.columns()-2);
        // // ignore under&overflows

        // DoubleMatrix2D errors = new
        // impl.DenseDoubleMatrix2D(toArrayErrors(h));
        // errors = errors.viewDice().viewRowFlip(); // keep the histo coord
        // system
        // //errors = errors.viewPart(1,1,errors.rows()-2,errors.columns()-2);
        // // ignore under&overflows

        return title
                + sep
                + "Heights:"
                + sep
                + new DoubleFormatter().toTitleString(null, yEdges, xEdges,
                        rowAxisName, columnAxisName, null, aggr);
        /*
         * + sep + "Errors:" + sep + new
         * doublealgo.Formatter().toTitleString(
         * errors,yEdges,xEdges,rowAxisName,columnAxisName,null,aggr);
         */
    }
    
}
