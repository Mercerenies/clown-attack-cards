import org.armedbear.lisp.*;
import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Queue;
import java.util.LinkedList;

public class MainPane extends JPanel implements Refreshable {
    private JPanel topRow;
    private JPanel midRow;
    private JPanel botRow;
    
    private ArrayList<PCard> bot;
    private Clownometer top;
    private Clock mid;
    
    private EventLog eventLog;
    
    public MainPane() {
        super();
        
        BoxLayout box = new BoxLayout(this, BoxLayout.Y_AXIS);
        this.setLayout(box);
        
        topRow = new JPanel();
        topRow.setLayout(new BorderLayout());
        this.add(topRow);
        
        midRow = new JPanel();
        midRow.setLayout(new BorderLayout());
        this.add(midRow);
        
        botRow = new JPanel();
        botRow.setLayout(new BoxLayout(botRow, BoxLayout.X_AXIS));
        this.add(botRow);
        
        JLabel elapsed = new JLabel("");
        midRow.add(elapsed, BorderLayout.WEST);
        
        top = new Clownometer();
        topRow.add(top, BorderLayout.EAST);
        
        eventLog = new EventLog();
        topRow.add(eventLog, BorderLayout.WEST);
        
        mid = new Clock(elapsed);
        midRow.add(mid, BorderLayout.CENTER);
        
        botRow.add(Box.createRigidArea(new Dimension(8, (int)Main.CARD.getHeight())));
        bot = new ArrayList<PCard>();
        
        this.refresh();
        
    }
    
    @Override
    public void refresh() {
        
        top.refresh();
        mid.refresh();
        
        refreshBottom();
        
    }
    
    private void refreshBottom() {
        
        botRow.removeAll();
        botRow.add(Box.createRigidArea(new Dimension(8, (int)Main.CARD.getHeight())));
        
        bot.clear();
        
        LispObject play = Main.getVar("*CARDS*");
        if (!(play instanceof Cons))
            return;
        LispObject[] crds = ((Cons)play).copyToArray();
        
        for (LispObject c : crds) {
            PCard cc = new PCard(c);
            botRow.add(cc);
            bot.add(cc);
        }

        botRow.revalidate();
        botRow.repaint();
        
        for (PCard pc : bot)
            pc.repaint();
        
    }

    public void writeEventLog(String s) {
        eventLog.write(s);
    }
    
    private class Clownometer extends JPanel implements Refreshable {
        int meter = 0;
        int cap = 0;
        
        public Clownometer() {
            super();
            
            this.setMinimumSize(new Dimension(128, 32));
            this.setPreferredSize(new Dimension(256, 64));
            this.setMaximumSize(new Dimension(256, 64));

            this.refresh();
            
        }
        
        @Override
        public void refresh() {
            
            LispObject var = Main.getVar("*CLOWN*");
            meter = Main.funcall("CLOWN-METER", var).intValue();
            cap = Main.funcall("CLOWN-CAP", var).intValue();
            
            this.repaint();
            
        }
        
        @Override
        public void paintComponent(Graphics g) {
            super.paintComponent(g);
            
            int x0 = 0;
            int x1 = this.getWidth();
            int y0 = this.getHeight() / 2 - 16;
            int y1 = y0 + 32;
            
            if (y0 < 0)
                y0 = 0;
            if (y1 > this.getHeight())
                y1 = this.getHeight();
            
            g.setColor(Color.BLACK);
            g.fillRect(x0, y0, x1 - x0, y1 - y0);
            
            double percent = (double)meter / (double)cap;
            int xMid = (int) (x1 * percent + x0 * (1 - percent));
            
            g.setColor(Color.RED);
            g.fillRect(x0, y0, xMid - x0, y1 - y0);
            
        }
        
    }
    
    private class Clock extends JPanel implements Refreshable {
        private int dayCount;
        private double minAngle;
        private double hrAngle;
        private JLabel elapsedLabel;
        
        public Clock(JLabel elapsed) {
            super();
            
            this.setMinimumSize(new Dimension(128, 128));
            this.setPreferredSize(new Dimension(256, 256));
            this.setMaximumSize(new Dimension(256, 256));
            
            this.elapsedLabel = elapsed;
            
            this.refresh();
            
        }
        
        @Override
        public void refresh() {
            LispObject var = Main.getVar("*TIME*");
            int days = Main.funcall("TIME-DAYS", var).intValue();
            int mins = Main.funcall("TIME-MINUTES", var).intValue();
            int total_mins = Main.funcall("MINUTES-IN-DAY",
                                          LispInteger.getInstance(days)).intValue();
            
            dayCount = days;
            
            double min_mod = (total_mins - mins) % 60;
            double hr_mod = (total_mins - mins) / 60D;
            
            minAngle = min_mod / 60D * 2 * Math.PI;
            hrAngle = hr_mod / 12D * 2 * Math.PI;
            
            minAngle += Math.PI / 2;
            hrAngle += Math.PI / 2;

            minAngle *= -1;
            hrAngle *= -1;
            
            int highscore = Main.getVar("*HIGHSCORE*").intValue();
            
            this.elapsedLabel
                .setText(String.format("<html>Days since last clown attack: %d<br />" +
                                       "(Longest run: %d)</html>",
                                       days,
                                       highscore));
            
            this.repaint();
            
        }
        
        @Override
        public void paintComponent(Graphics g) {
            super.paintComponent(g);
            
            int width = this.getWidth();
            int height = this.getHeight();
            
            double xx = width / 2D;
            double yy = height / 2D;
            
            double r = Math.min(xx, yy);
            
            g.setColor(Color.WHITE);
            g.fillOval((int)(xx - r), (int)(yy - r), 2 * (int)r, 2 * (int)r);
            
            g.setColor(Color.BLACK);
            g.drawOval((int)(xx - r), (int)(yy - r), 2 * (int)r, 2 * (int)r);
            
            g.drawLine((int)xx, (int)yy,
                       (int)(xx + r * Math.cos(minAngle)),
                       (int)(yy + r * Math.sin(minAngle)));
            g.drawLine((int)xx, (int)yy,
                       (int)(xx + r / 2 * Math.cos(hrAngle)),
                       (int)(yy + r / 2 * Math.sin(hrAngle)));
            
        }
        
    }
    
    private class PCard extends CardLabel {
        private JPanel textPanel;
        private JLabel textLabel;
        
        public PCard(LispObject card) {
            super(card, "FIELD-NAME");
            
            textLabel = new JLabel("");
            textLabel.setAlignmentX(JPanel.CENTER_ALIGNMENT);
            textLabel.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 14));
            textLabel.setForeground(Color.RED);
            textLabel.setBackground(Color.BLACK);
            textLabel.setOpaque(true);
            
            textPanel = new JPanel();
            textPanel.setOpaque(false);
            
            this.setLayout(new BorderLayout());
            this.add(textPanel, BorderLayout.SOUTH);
            
            textPanel.setLayout(new BorderLayout());
            textPanel.add(textLabel, BorderLayout.EAST);
            
            Main.getFrame().addDescListener(this);
            refreshIcon();
            
        }
        
        @Override
        protected void refreshIcon() {
            super.refreshIcon();
            
            if (textLabel != null) {
                LispObject card = this.getCard();
                if (card.typep(Main.getSymbol("COUNTER")).getBooleanValue()) {
                    LispObject counter = Main.funcall("COUNTDOWN", card);
                    textLabel.setText(Integer.toString(counter.intValue()));
                } else {
                    textLabel.setText("");
                }
            }
            
        }
        
    }
    
    private class EventLog extends JLabel {
        private Queue<String> log;
        
        public static final int MAX_LOG = 5;
        
        public EventLog() {
            super("");
            
            log = new LinkedList<String>();
            
            for (int i = 0; i < MAX_LOG; i++)
                write("");
            
            refreshText();
            
        }
        
        public void write(String s) {
            log.add(s);
            while (log.size() > MAX_LOG)
                log.remove();
            refreshText();
        }
        
        private void refreshText() {
            final String INIT = "<html>";
            final String CONC = "</html>";
            int n = 0;
            StringBuilder sb = new StringBuilder(INIT);
            StringBuilder temp = null;
            for (String s : log) {
                temp = new StringBuilder();
                if (n == MAX_LOG - 1)
                    temp.append(":: ");
                else
                    temp.append("- ");
                temp.append(s).append("<br />");
                sb.insert(INIT.length(), temp);
                n++;
            }
            String end = sb.append(CONC).toString();
            this.setText(end);
        }
        
    }
    
}
