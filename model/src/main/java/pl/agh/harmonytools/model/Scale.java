package pl.agh.harmonytools.model;

public abstract class Scale {

    protected int[] pitches;
    protected Key key;

    protected Scale(Key key) {
        this.key = key;
    }

    protected Scale(BaseNote baseNote, Integer tonicPitch) {
        this.key = new Key(baseNote, tonicPitch);
    }

}