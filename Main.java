import org.armedbear.lisp.*;
import org.armedbear.lisp.Package;
import java.awt.*;
import javax.swing.*;

/////
//    Encrypt save file
// -- Automate build process
// -- Remove build code from Loading.java
// -- Proper loading screen when run w/o terminal
//    Possibly error handling for arbitrary JAVA exceptions

public class Main {
    public static final Dimension CARD = new Dimension(128, 128);
    public static final int CLOWN_INDEX = 0;
    
    private static Interpreter lisp = null;
    private static Package pkg = null;
    private static Gameplay game = null;
    private static MyFrame frame = null;
    
    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    Loading.startInterp();
                }
            });
    }

    private Main() {}
    
    public static void start(Interpreter i) {
        
        lisp = i;
        pkg = Packages.findPackage("CARDGAME");
        
        game = new Gameplay();
        frame = new MyFrame();
        
    }
    
    public static Interpreter getLisp() {
        return lisp;
    }
    
    public static Package getPackage() {
        return pkg;
    }
    
    public static Gameplay getGameplay() {
        return game;
    }

    public static MyFrame getFrame() {
        return frame;
    }
    
    public static Symbol getSymbol(String name) {
        return pkg.findAccessibleSymbol(name);
    }
    
    public static LispObject getVar(String name) {
        Symbol sym = pkg.findAccessibleSymbol(name);
        if (sym == null)
            return null;
        else
            return sym.getSymbolValue();
    }

    private static LispObject getFuncallable(String name) {
        Symbol sym = pkg.findAccessibleSymbol(name);
        if (sym == null)
            return null;
        LispObject fun = sym.getSymbolFunction();
        if (fun == null)
            return null;
        return fun;
    }

    public static LispObject funcall(String name) {
        LispObject fun = getFuncallable(name);
        if (fun == null)
            return null;
        return fun.execute();
    }
    
    public static LispObject funcall(String name, LispObject arg0) {
        LispObject fun = getFuncallable(name);
        if (fun == null)
            return null;
        return fun.execute(arg0);
    }

    public static LispObject funcall(String name, LispObject arg0, LispObject arg1) {
        LispObject fun = getFuncallable(name);
        if (fun == null)
            return null;
        return fun.execute(arg0, arg1);
    }

    public static LispObject funcall(String name, LispObject arg0, LispObject arg1,
                                                  LispObject arg2) {
        LispObject fun = getFuncallable(name);
        if (fun == null)
            return null;
        return fun.execute(arg0, arg1, arg2);
    }
    
    public static LispObject funcallLog(String name) {
        LispObject wrap = getFuncallable("CALL-WITH-HANDLER");
        LispObject fun = getFuncallable(name);
        if ((wrap == null) || (fun == null))
            return null;
        return wrap.execute(fun);
    }
    
    public static LispObject funcallLog(String name, LispObject arg0) {
        LispObject wrap = getFuncallable("CALL-WITH-HANDLER");
        LispObject fun = getFuncallable(name);
        if ((wrap == null) || (fun == null))
            return null;
        return wrap.execute(fun, arg0);
    }

    public static void assignCardDims(JComponent j) {
        j.setMinimumSize(new Dimension(32, 32));
        j.setPreferredSize(new Dimension(128, 128));
        j.setMaximumSize(new Dimension(128, 128));
    }

    public static void writeEventLog(String s) {
        frame.writeEventLog(s);
    }
    
    public static void doClownAttack() {
        GlassPane glass = frame.getGlassLayer();
        int width = glass.getWidth();
        glass.setDelta(width / 25);
    }
    
}
