class Foo {
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("<");
    sb.append(this.getClass().getSimpleName());
    sb.append(" ");

    if (getSetField() != null) {
      Object v = EXTRAVAGANCE_SENSITIVE_FIELDS.contains(this.getSetField()) ? "<redacted>" : this.getFieldValue();

      sb.append(getFieldDesc(getSetField()).name);
      sb.append(":");
      if(v instanceof java.nio.ByteBuffer) {
        org.apache.thrift.TBaseHelper.toString((java.nio.ByteBuffer)v, sb);
      } else {
        sb.append(v.toString());
      }
    }
    sb.append(">");
    return sb.toString();
  }
}
