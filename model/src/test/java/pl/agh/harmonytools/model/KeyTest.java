package pl.agh.harmonytools.model;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class KeyTest {

    @Test
    public void baseNoteTest() {
        Key G = new Key("G##");
        Assertions.assertEquals(G.getBaseNote(), BaseNote.G);
    }

    @Test
    public void notSupportedKeyTest() {
        Assertions.assertThrows(IllegalArgumentException.class, () -> new Key("R#"));
    }
}
