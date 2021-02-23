package pl.agh.harmonytools.model;

public enum BaseNote {

    C(0),
    D(1),
    E(2),
    F(3),
    G(4),
    A(5),
    B(6);

    private int value;

    BaseNote(int value) {
        this.value = value;
    }

    private int getValue() {
        return value;
    }

}
