import javax.swing.*;
import java.awt.*;
import java.util.Timer;
import java.util.TimerTask;

public class GlassPane extends JComponent {
    public static final int TIMER_DELTA = 20;
    
    private Timer timer;
    private TimerTask task;
    private double imageSize;
    private double imageSizeDelta;
    
    public GlassPane() {
        super();
        imageSize = 0D;
        imageSizeDelta = 0D;
        
        timer = new Timer();
        task = new Task();
        timer.schedule(task, TIMER_DELTA);

        this.setVisible(true);
        this.repaint();
        
    }
    
    @Override
    public void paintComponent(Graphics g) {
        int width = this.getWidth();
        int height = this.getHeight();
        
        if (imageSize > 0D) {
            Image img = Images.getInstance().getImage(Main.CLOWN_INDEX);
            g.drawImage(img, (int)(width - imageSize) / 2, (int)(height - imageSize) / 2,
                        (int)imageSize, (int)imageSize,
                        Color.WHITE, null);
        }
        
    }
    
    public void step() {
        int maxSize = this.getWidth() / 2;
        imageSize += imageSizeDelta;
        if (imageSize > maxSize) {
            imageSize = 0D;
            imageSizeDelta = 0D;
        }
        this.repaint();
    }

    public void setDelta(double delta) {
        imageSizeDelta = delta;
    }
    
    public void purgeTimer() {
        timer.purge();
        timer = null;
    }

    private class Task extends TimerTask {
        
        @Override
        public void run() {
            SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        step();
                    }
                });
            if (timer != null) {
                task = new Task();
                timer.schedule(task, TIMER_DELTA);
            }
        }
        
    }
    
}
