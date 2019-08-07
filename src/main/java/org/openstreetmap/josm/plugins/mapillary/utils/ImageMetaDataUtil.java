package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.Imaging;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.XmlUtils;


public class ImageMetaDataUtil {

  private ImageMetaDataUtil() {
    // private util.
  }

  public static boolean getPanorama(final InputStream is) {
    String xmpxml = null;
    try {
      xmpxml = Imaging.getXmpXml(is, null);
    } catch (ImageReadException | IOException ex) {
      Logging.trace(ex);
      return false;
    }
    boolean pano;
    if (xmpxml != null) {
      pano= getPanorama(new StringReader(xmpxml));
    } else {
      pano = false;
    }
    return pano;
  }

  public static boolean getPanorama(final Reader sr) {
    boolean pano = false;
    try {
      DocumentBuilder builder = XmlUtils.newSafeDOMBuilder();
      Document document = builder.parse(new InputSource(sr));
      Element root = document.getDocumentElement();
      NodeList xmpMetaNodeList = root.getChildNodes();
      for (int i = 0; i < xmpMetaNodeList.getLength(); i++) {
        Node rdfNode = xmpMetaNodeList.item(i);
        if (rdfNode.getNodeType() == Node.ELEMENT_NODE) {
          Element rdfElement = (Element) rdfNode;
          NodeList rdfChildNodeList = rdfElement.getChildNodes();
          for (int j = 0; j < rdfChildNodeList.getLength(); j++) {
            Node rdfChildNode = rdfChildNodeList.item(j);
            if (rdfChildNode.getNodeType() == Node.ELEMENT_NODE) {
              Element rdfChildElement = (Element) rdfChildNode;
              String projection = rdfChildElement.getAttributeNS("http://ns.google.com/photos/1.0/panorama/", "ProjectionType");
              if ("equirectangular".equals(projection)) {
                pano = true;
                break;
              }
            }
          }
        }
      }
    } catch (ParserConfigurationException | SAXException | IOException ex) {
      Logging.trace(ex);
    }
    return pano;
  }
}
