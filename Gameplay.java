import org.armedbear.lisp.*;

public class Gameplay {
    
    public Gameplay() {
        Main.funcall("INIT-DICT");
        Main.funcall("LOAD-HIGHSCORE");
        
        // The game seems to have problems refreshing the first time we
        // draw, so go ahead and do that once to get it out of the way.
        Main.funcall("DRAW-CARD");
        Symbol sym = Main.getSymbol("*PLAYER-HAND*");
        sym.setSymbolValue(Lisp.NIL);
        
    }
    
    public void drawCard() {
        LispObject success = Main.funcallLog("DRAW-CARD");
        if (success.getBooleanValue())
            Main.funcallLog("END-OF-TURN");
        Main.getFrame().refresh();
    }
    
    public void useCard(LispObject obj) {
        Main.funcallLog("USE-CARD", obj);
        Main.funcallLog("END-OF-TURN");
        Main.getFrame().refresh();
    }
    
}
