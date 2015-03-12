import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import org.armedbear.lisp.*;

public class MyFrame extends JFrame implements Refreshable {
    private JPanel lowerPanel;
    private CardPanel cardPanel;
    private DeckLabel deckPanel;
    private DescLabel descLabel;
    private MainPane mainPane;
    private JPanel cardHolder;
    private GlassPane glass;
    
    private MyListener listener;
    
    public MyFrame() {
        super("Card Game");
        
        //this.setLayout(new BorderLayout());
        
        listener = new MyListener();
        
        glass = new GlassPane();
        this.setGlassPane(glass);
        
        lowerPanel = new JPanel();
        lowerPanel.setLayout(new BorderLayout());
        this.add(lowerPanel, BorderLayout.SOUTH);
        
        cardHolder = new JPanel();
        cardHolder.setLayout(new BoxLayout(cardHolder, BoxLayout.Y_AXIS));
        lowerPanel.add(cardHolder, BorderLayout.CENTER);
        
        cardPanel = new CardPanel();
        cardHolder.add(cardPanel);

        deckPanel = new DeckLabel();
        lowerPanel.add(deckPanel, BorderLayout.EAST);
        
        descLabel = new DescLabel();
        lowerPanel.add(descLabel, BorderLayout.WEST);

        mainPane = new MainPane();
        this.add(mainPane, BorderLayout.CENTER);
        
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        this.getRootPane().getInputMap().put(KeyStroke.getKeyStroke("released SPACE"),
                                             "drawCard");
        this.getRootPane().getActionMap().put("drawCard",
                                              listener);
        
        this.pack();
        this.setVisible(true);
        this.getGlassPane().setVisible(true);
        
        Runtime.getRuntime().addShutdownHook(new Thread() {
                public void run() {
                    glass.purgeTimer();
                }
            });
        
    }
    
    @Override
    public void refresh() {
        
        mainPane.refresh();
        cardPanel.refresh();
        
    }
    
    public GlassPane getGlassLayer() {
        return glass;
    }

    public void setDescText(String s) {
        descLabel.setText(s);
    }

    public void writeEventLog(String s) {
        mainPane.writeEventLog(s);
    }

    public void addDescListener(Component com) {
        com.addMouseListener(listener);
    }

    private class MyListener extends AbstractAction implements MouseListener{
        
        private MyListener() {
            
        }
        
        @Override
        public void actionPerformed(ActionEvent ev) {
            Main.getGameplay().drawCard();
        }
        
        @Override
        public void mouseClicked(MouseEvent ev) {
            Component com = ev.getComponent();
            if (com instanceof DeckLabel) {
                Main.getGameplay().drawCard();
            } else if (com instanceof Card) {
                LispObject cur = ((Card)com).getCard();
                Main.getGameplay().useCard(cur);
            } else {
                // Do nothing
            }
        }
        
        @Override
        public void mouseEntered(MouseEvent ev) {
            Component com = ev.getComponent();
            if (com instanceof Describable) {
                String s = ((Describable)com).describe();
                Main.getFrame().setDescText(new StringBuilder()
                    .append("<html>").append(s).append("</html>")
                    .toString()
                );
            }
        }
        
        @Override
        public void mouseExited(MouseEvent ev) {
            Main.getFrame().setDescText("");
        }

        @Override
        public void mousePressed(MouseEvent ev) {}

        @Override
        public void mouseReleased(MouseEvent ev) {}
        
    }
    
    private class DeckLabel extends JLabel implements Describable {
        public static final int DECK_INDEX = 27;
        
        public DeckLabel() {
            super();
            
            Main.assignCardDims(this);
            this.addMouseListener(listener);
            
            this.setIcon(new ImageIcon(Images.getInstance().getImage(DECK_INDEX)));
            
            this.setHorizontalAlignment(SwingConstants.CENTER);
            this.setBorder(BorderFactory.createLineBorder(Color.BLACK));
            
        }
        
        @Override
        public String describe() {
            int mx = Main.getVar("*HAND-SIZE*").intValue();
            return String.format("Deck - Draw a card from the deck (max hand size %d)",
                                 mx);
        }
        
    }
    
    private class CardPanel extends JPanel implements Refreshable {
        private ArrayList<Card> cards;
        
        public CardPanel() {
            super();
            
            BoxLayout box = new BoxLayout(this, BoxLayout.X_AXIS);
            this.setLayout(box);

            int maxCards = Main.getVar("*HAND-SIZE*").intValue();
            this.setPreferredSize(new Dimension((int)Main.CARD.getWidth() * (maxCards + 2),
                                                (int)Main.CARD.getHeight()));
            
            cards = new ArrayList<Card>();
            
            this.refresh();
            
        }
        
        @Override
        public void refresh() {
            
            this.removeAll();
            this.add(Box.createRigidArea(new Dimension(8, (int)Main.CARD.getHeight())));
            
            LispObject play = Main.getVar("*PLAYER-HAND*");
            
            if (!(play instanceof Cons))
                return;
            LispObject[] crds = ((Cons)play).copyToArray();
        
            for (LispObject c : crds) {
                Card cc = new Card(c);
                this.add(cc);
                cards.add(cc);
            }
            
            if (cards.isEmpty()) {
                JLabel jj = new JLabel("");
                Main.assignCardDims(jj);
                this.add(jj); // Ensure that the draw is performed
            }
            
            this.revalidate();
            this.repaint();
            
            for (Card c : cards)
                c.repaint();
            
        }
        
    }
    
    private class Card extends CardLabel {
        
        public Card(LispObject card) {
            super(card);
            this.addMouseListener(listener);
        }
        
    }
    
    private class DescLabel extends JLabel {
        
        public DescLabel() {
            super();
            
            this.setText("");
            
            Dimension dim = new Dimension((int)Main.CARD.getWidth() * 2,
                                          (int)Main.CARD.getHeight());
            this.setMinimumSize(dim);
            this.setPreferredSize(dim);
            
        }
        
    }
    
}
