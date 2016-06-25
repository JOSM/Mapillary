// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import org.openstreetmap.josm.tools.I18n;

public final class ValidationUtil {
  private static final String KEY_REGEX = "[a-zA-Z0-9\\-_]{22}";

  private ValidationUtil() {
    // Empty private constructor to avoid instantiation
  }

  /**
   * Checks if the given image key matches the expected format (22 characters, that can be alphanumeric,
   * <code>-</code> or <code>_</code>).
   * @param imgKey the image key that should be validated
   * @return <code>true</code> iff the image key matches the expected format or if it is <code>null</code>,
   *           otherwise <code>false</code>.
   */
  public static boolean validateImageKey(String imgKey) {
    return imgKey != null && imgKey.matches(KEY_REGEX);
  }

  /**
   * Checks if the given sequence key matches the expected format (same as in {@link #validateImageKey(String)}).
   * This method is completely the same as {@link #validateImageKey(String)}, but I wanted to separate the validation
   * of image and sequence keys.
   * @param seqKey the sequence key that should be validated
   * @return <code>true</code> iff the sequence key matches the expected format or if it is <code>null</code>,
   *           otherwise <code>false</code>.
   * @see {@link #validateImageKey(String)}
   */
  public static boolean validateSequenceKey(String seqKey) {
    return validateImageKey(seqKey);
  }

  /**
   * Validates the image key and throws an {@link IllegalArgumentException} when it is not valid.
   * @param imgKey the image key to validate
   * @param nullAllowed this controls the behaviour when the key is <code>null</code>. If this variable is
   *          <code>false</code>, an {@link IllegalArgumentException} is then thrown, otherwise nothing is done.
   * @see {@link #validateImageKey(String)}
   */
  public static void throwExceptionForInvalidImgKey(String imgKey, boolean nullAllowed) {
    if (!validateSequenceKey(imgKey)) {
      throw new IllegalArgumentException(I18n.tr("The image key ''{0}'' is invalid!", imgKey));
    }
    if (!nullAllowed && imgKey == null) {
      throw new IllegalArgumentException(I18n.tr("The image key must not be null here!"));
    }
  }

  /**
   * Validates the sequence key and throws an {@link IllegalArgumentException} when it is not valid.
   * @param seqKey the sequence key to validate
   * @param nullAllowed this controls the behaviour when the key is <code>null</code>. If this variable is
   *          <code>false</code>, an {@link IllegalArgumentException} is then thrown, otherwise nothing is done.
   * @see {@link #validateSequenceKey(String)}
   */
  public static void throwExceptionForInvalidSeqKey(String seqKey, boolean nullAllowed) {
    if (!validateSequenceKey(seqKey)) {
      throw new IllegalArgumentException(I18n.tr("The sequence key ''{0}'' is invalid!", seqKey));
    }
    if (!nullAllowed && seqKey == null) {
      throw new IllegalArgumentException(I18n.tr("The sequence key must not be null here!"));
    }
  }
}
