package org.openstreetmap.josm.plugins.mapillary;

import java.util.Date;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;

/**
 * Abstract superclass for all image objects. At the moment there is just 2,
 * {@code MapillaryImportedImage} and {@code MapillaryImage}.
 * 
 * @author nokutu
 *
 */
public abstract class MapillaryAbstractImage {

    public static Lock lock = new ReentrantLock();

    private long capturedAt;

    /** Position of the picture */
    public final LatLon latLon;
    /** Direction of the picture */
    public final double ca;
    public boolean isModified = false;
    /** Temporal position of the picture until it is uploaded */
    public LatLon tempLatLon;
    /**
     * When the object is being dragged in the map, the temporal position is
     * stored here
     */
    public LatLon movingLatLon;
    /** Temporal direction of the picture until it is uploaded */
    public double tempCa;
    /**
     * When the object direction is being moved in the map, the temporal
     * direction is stored here
     */
    protected double movingCa;
    private boolean visible;

    public MapillaryAbstractImage(double lat, double lon, double ca) {
        this.latLon = new LatLon(lat, lon);
        this.tempLatLon = this.latLon;
        this.movingLatLon = this.latLon;
        this.ca = ca;
        this.tempCa = ca;
        this.movingCa = ca;
        this.visible = true;
    }

    /**
     * Returns whether the object has been modified or not.
     * 
     * @return true if the object has been modified; false otherwise.
     */
    public boolean isModified() {
        return this.isModified;
    }

    /**
     * Returns a LatLon object containing the current coordinates of the object.
     * When you are dragging the image this changes.
     * 
     * @return The LatLon object with the position of the object.
     */
    public LatLon getLatLon() {
        return movingLatLon;
    }

    /**
     * Returns whether the image is visible on the map or not.
     * 
     * @return
     */
    public boolean isVisible() {
        return visible;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    /**
     * Returns the last fixed coordinates of the object.
     * 
     * @return
     */
    public LatLon getTempLatLon() {
        return tempLatLon;
    }

    /**
     * Moves the image temporally to another position
     * 
     * @param x
     * @param y
     */
    public void move(double x, double y) {
        this.movingLatLon = new LatLon(this.tempLatLon.getY() + y,
                this.tempLatLon.getX() + x);
        this.isModified = true;
    }

    /**
     * Turns the image direction.
     * 
     * @param ca
     */
    public void turn(double ca) {
        this.movingCa = this.tempCa + ca;
        this.isModified = true;
    }

    /**
     * Called when the mouse button is released, meaning that the picture has
     * stopped being dragged.
     */
    public void stopMoving() {
        this.tempLatLon = this.movingLatLon;
        this.tempCa = this.movingCa;
    }

    /**
     * Returns the direction towards the image has been taken.
     * 
     * @return The direction of the image (0 means north and goes clockwise).
     */
    public double getCa() {
        return movingCa;
    }

    /**
     * Returns the last fixed direction of the object.
     * 
     * @return
     */
    public double getTempCa() {
        return tempCa;
    }

    /**
     * Returns the date the picture was taken in DMY format.
     * 
     * @return
     */
    public String getDate() {
        String format = "";
        if (Main.pref.getBoolean("iso.dates"))
            format += "yyyy-MM-dd";
        else
            format += "dd/MM/yyyy";
        if (Main.pref.getBoolean("mapillary.display-hour", true)) {
            if (Main.pref.getBoolean("mapillary.format-24"))
                format += " - HH:mm:ss (z)";
            else
                format += " - h:mm:ss a (z)";
        }
        return getDate(format);
    }

    public void setCapturedAt(long capturedAt) {
        this.capturedAt = capturedAt;
    }

    public long getCapturedAt() {
        return capturedAt;
    }

    /**
     * Returns the date the picture was taken in the given format.
     * 
     * @param format
     * @return
     */
    public String getDate(String format) {
        Date date = new Date(getCapturedAt());

        SimpleDateFormat formatter = new SimpleDateFormat(format);
        formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
        return formatter.format(date);
    }

    /**
     * Parses a string with a given format and returns the Epoch time.
     * 
     * @param date
     * @param format
     * @return
     */
    public long getEpoch(String date, String format) {

        SimpleDateFormat formatter = new SimpleDateFormat(format);
        try {
            Date dateTime = (Date) formatter.parse(date);
            return dateTime.getTime();
        } catch (ParseException e) {
            Main.error(e);
        }
        return currentTime();
    }

    /**
     * Returns current time in Epoch format
     * 
     * @return
     */
    private long currentTime() {
        Calendar cal = Calendar.getInstance();
        return cal.getTimeInMillis();
    }
}