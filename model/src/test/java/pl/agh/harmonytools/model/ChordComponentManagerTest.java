package pl.agh.harmonytools.model;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import pl.agh.harmonytools.model.util.ChordComponentManager;

public class ChordComponentManagerTest {

    @Test
    public void instancesCheck() {
        ChordComponent cc1 = ChordComponentManager.chordComponentFromString("5>", false);
        ChordComponent cc2 = ChordComponentManager.chordComponentFromString("5>", false);
        Assertions.assertEquals(cc1, cc2);

        cc1 = ChordComponentManager.chordComponentFromString("5>", true);
        cc2 = ChordComponentManager.chordComponentFromString("5>", false);
        Assertions.assertNotEquals(cc1, cc2);

        cc1 = ChordComponentManager.chordComponentFromString("5>", true);
        cc2 = ChordComponentManager.chordComponentFromString("5>", true);
        Assertions.assertEquals(cc1, cc2);
    }
}
