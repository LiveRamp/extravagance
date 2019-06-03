@Override
public String toString() {
  StringBuilder sb = new StringBuilder();
  sb.append("<");
  sb.append(this.getClass().getSimpleName());
  sb.append(" ");

  if (getSetField() != null) {
    Object v = sensitiveFields.contains(this.getSetField()) ? "redacted" : this.getFieldValue();

    sb.append(getFieldDesc(getSetField()).name);
    sb.append(":");
    if(v instanceof ByteBuffer) {
      TBaseHelper.toString((ByteBuffer)v, sb);
    } else {
      sb.append(v.toString());
    }
  }
  sb.append(">");
  return sb.toString();
}
