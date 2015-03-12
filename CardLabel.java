import org.armedbear.lisp.*;
import javax.swing.*;
import java.awt.*;

public class CardLabel extends JLabel implements Describable {
    private String nameFormat;
    private LispObject card;
    
    public CardLabel(LispObject card) {
        this(card, "CARD-NAME");
    }
    
    public CardLabel(LispObject card, String format) {
        super();
            
        Main.assignCardDims(this);
        
        this.card = card;
        this.nameFormat = format;
        
        refreshIcon();
            
    }
    
    protected void refreshIcon() {
        /*
        if (card == null) {
            this.setText("Null");
            return;
        }
        String text = Main.funcall(nameFormat, card).getStringValue();
        if (text == null)
            this.setText("Unknown");
        else
            this.setText(text);
        */
        this.setIcon(new ImageIcon(Images.getInstance().getImageFor(card)));
        this.setHorizontalAlignment(SwingConstants.CENTER);
        this.setBorder(BorderFactory.createLineBorder(Color.BLACK));
    }
    
    public LispObject getCard() {
        return card;
    }
    
    public void setCard(LispObject card) {
        this.card = card;
        refreshIcon();
    }

    @Override
    public String describe() {
        LispObject name = Main.funcall("CARD-NAME", this.getCard());
        LispObject desc = Main.funcall("CARD-DESC", this.getCard());
        return String.format("%s - %s", name, desc);
    }
    
}
