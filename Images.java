import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
import org.armedbear.lisp.LispObject;

public class Images {
    private static Images instance = new Images();
    public static final int IMAGE_SIZE = 16;
    
    private BufferedImage basis;
    private int imageCount;
    private Image[] images;
    
    private Images() {
        try {
            basis = ImageIO.read(new File("cards.png"));
            imageCount = basis.getWidth() / IMAGE_SIZE;
        } catch (IOException e) {
            e.printStackTrace();
            basis = null;
            imageCount = 0;
        }
        
        images = new Image[imageCount];
        
    }
    
    private void loadImage(int index) {
        BufferedImage temp = basis.getSubimage(IMAGE_SIZE * index, 0,
                                               IMAGE_SIZE, IMAGE_SIZE);
        images[index] = temp.getScaledInstance((int)Main.CARD.getWidth(),
                                               (int)Main.CARD.getHeight(),
                                               0);
    }
    
    public Image getImage(int index) {
        try {
            Image img = images[index];
            if (img == null) {
                loadImage(index);
                img = images[index];
            }
            return img;
        } catch (IndexOutOfBoundsException e) {
            return null;
        }
    }
    
    public Image getImageFor(LispObject card) {
        LispObject integer = Main.funcall("CARD-IMAGE", card);
        if (integer == null)
            return null;
        return getImage(integer.intValue());
    }
    
    public static Images getInstance() {
        return instance;
    }
    
    
    
}
