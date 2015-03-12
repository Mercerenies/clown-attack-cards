import org.armedbear.lisp.*;
import java.io.File;
import javax.swing.*;

public class Loading extends SwingWorker<Interpreter, Void> {
    public static final boolean DEBUG_GAME = false;
    public static final String[] files = new String[]{"package", "macro", "deck",
                                                      "arbitrary", "lists", "log",
                                                      "option", "event", "counter",
                                                      "behavior", "field", "game",
                                                      "cards", "bridge", "data"};
    
    private ProgressMonitor monitor;
    private Interpreter interpreter;
    private int count;

    private Loading() {
        monitor = new ProgressMonitor(null, "Loading game...",
                                      "Starting interpreter process...",
                                      0, files.length);
        interpreter = Interpreter.createInstance();
        count = 0;
    }
    
    private void loadFile(String s) {
        String prefix = "jar:file:Logic.jar!/";
        String lispName = String.format("%s.lisp", s);
        String compName = String.format("%s.abcl", s);
        monitor.setNote(String.format("Loading %s.lisp...%n", s));
        if (!(new File(compName)).exists()) {
            if (DEBUG_GAME)
                interpreter.eval(String.format("(compile-file \"%s\")", lispName));
            /*
            else
                throw new RuntimeException(String.format("%s not found!",
                                                         compName));
            */
        }
        if (DEBUG_GAME)
            interpreter.eval(String.format("(load \"%s\")", compName));
        else
            interpreter.eval(String.format("(load #p\"%s%s\")", prefix, compName));
        count++;
        monitor.setProgress(count);
    }

    @Override
    public Interpreter doInBackground() {
        monitor.setMillisToPopup(100);
        monitor.setNote("Loading card data...");
        for (String s : files)
            loadFile(s);
        //System.out.println("Done.");
        return interpreter;
    }

    @Override
    protected void done() {
        monitor.setProgress(0);
        monitor.close();
        Main.start(interpreter);
    }
    
    public static void startInterp() {
        (new Loading()).execute();
    }
    
}
